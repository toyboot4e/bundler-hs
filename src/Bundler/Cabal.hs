module Bundler.Cabal
  ( ProjectDefaults (..),
    ProjectFlags,
    findProjectDefaults,
    findProjectFlags,
  )
where

import Bundler.Error
import Bundler.Utf8 (readUtf8File)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Distribution.Compiler (CompilerFlavor (GHC))
import Distribution.Package (pkgName, unPackageName)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty (prettyShow)
import Distribution.System (buildArch, buildOS)
import Distribution.Types.Benchmark (benchmarkBuildInfo)
import Distribution.Types.BuildInfo (BuildInfo, cppOptions, defaultExtensions, defaultLanguage)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Executable (buildInfo)
import Distribution.Types.Flag (FlagName, flagDefault, flagName, mkFlagName)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library (libBuildInfo)
import Distribution.Types.PackageDescription (package)
import Distribution.Types.TestSuite (testBuildInfo)
import Distribution.Version (Version, mkVersion, withinRange)
import System.Directory (canonicalizePath, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))

-- | The language defaults a source tree inherits from its enclosing cabal
-- project. The defaults are synthesized into LANGUAGE pragma lines so that
-- one code path (GHC's own pragma parser) interprets cabal defaults, and the
-- same lines can be re-emitted into the bundle's pragma block.
data ProjectDefaults = ProjectDefaults
  { -- | Directory holding the @.cabal@ file, or the starting directory when no
    -- project was found.
    pdRoot :: FilePath,
    -- | Synthesized @{-\# LANGUAGE ... \#-}@ lines: @default-language@ first
    -- (editions like GHC2021 are valid pragmas), then @default-extensions@,
    -- unioned over every stanza and conditional branch.
    pdPragmas :: [String],
    -- | CPP macros from @cpp-options: -DNAME[=VALUE]@, resolved the way
    -- cabal would resolve them for a default build: @if flag(x)@ takes the
    -- flag's declared default, and @os@\/@arch@\/@impl(ghc)@ are decided
    -- against this host and the GHC version whose @__GLASGOW_HASKELL__@ we
    -- claim. A condition that cannot be decided contributes both branches,
    -- like the language defaults.
    pdDefines :: [(String, String)]
  }
  deriving (Show)

-- | Flag assignments a cabal project file imposes, keyed by package name.
-- These override a package's own declared flag defaults, the way
-- @constraints:@ and @package ... flags:@ do for a real build.
type ProjectFlags = Map String (Map FlagName Bool)

-- | The flag assignments governing a build started from @start@.
--
-- Searches upward for @cabal.project@, layering @cabal.project.freeze@ and
-- @cabal.project.local@ on top exactly as cabal does, later files winning.
-- Freeze files matter: they record flags as well as versions, for instance
-- @QuickCheck -old-random +templatehaskell@.
--
-- The search stops at the directory holding the package's own @.cabal@, so
-- an unrelated project file further up cannot reach in. A project file that
-- sits strictly above its packages, as in some multi-package repositories,
-- is therefore not picked up.
findProjectFlags :: FilePath -> IO ProjectFlags
findProjectFlags start = canonicalizePath start >>= go
  where
    go dir = do
      let projectFile = dir </> "cabal.project"
      hasProject <- doesFileExist projectFile
      if hasProject
        then do
          texts <-
            traverse
              readIfPresent
              [projectFile, projectFile <.> "freeze", projectFile <.> "local"]
          pure (foldl' layer Map.empty (map parseProjectFlags (concat texts)))
        else do
          cabals <- filter ((== ".cabal") . takeExtension) <$> listDirectory dir
          let parent = takeDirectory dir
          if not (null cabals) || parent == dir
            then pure Map.empty
            else go parent

    readIfPresent path = do
      there <- doesFileExist path
      if there then (: []) <$> readUtf8File path else pure []

    -- Later files win, per package and per flag.
    layer older newer = Map.unionWith (flip Map.union) older newer

-- | Walk upward from @start@ (a directory) to the first directory containing
-- a @.cabal@ file. No project at all is fine (empty defaults); more than one
-- @.cabal@ in the same directory is an error.
findProjectDefaults :: ProjectFlags -> FilePath -> IO (Either BundleError ProjectDefaults)
findProjectDefaults projFlags start = canonicalizePath start >>= go
  where
    go dir = do
      cabals <- filter ((== ".cabal") . takeExtension) <$> listDirectory dir
      case cabals of
        [name] -> readDefaults projFlags dir (dir </> name)
        [] ->
          let parent = takeDirectory dir
           in if parent == dir
                then pure (Right (ProjectDefaults start [] []))
                else go parent
        several -> pure (Left (AmbiguousCabal dir several))

readDefaults :: ProjectFlags -> FilePath -> FilePath -> IO (Either BundleError ProjectDefaults)
readDefaults projFlags root path = do
  contents <- BS.readFile path
  pure $ case parseGenericPackageDescriptionMaybe contents of
    Nothing -> Left (CabalError path "not a valid package description")
    Just gpd ->
      Right
        ProjectDefaults
          { pdRoot = root,
            pdPragmas = map mkPragma (nubOrd (languages <> extensions)),
            pdDefines =
              nubOrd (concatMap (mapMaybe parseDefine . cppOptions) (resolvedBuildInfos imposed gpd))
          }
      where
        infos = allBuildInfos gpd
        languages = map prettyShow (concatMap (maybeToList . defaultLanguage) infos)
        extensions = map prettyShow (concatMap defaultExtensions infos)
        imposed =
          Map.findWithDefault
            Map.empty
            (unPackageName (pkgName (package (packageDescription gpd))))
            projFlags

-- | Pull @constraints:@ entries and @package NAME@ / @flags:@ stanzas out of
-- a cabal project file. Version constraints and every other field are
-- ignored, so @any.foo ==1.0@ contributes nothing while @foo +bar@ does.
parseProjectFlags :: String -> ProjectFlags
parseProjectFlags src = Map.unionsWith Map.union (constraintFlags <> packageFlags)
  where
    blocks = topLevelBlocks (map stripComment (lines src))

    constraintFlags =
      [ Map.singleton name flags
      | (header, body) <- blocks,
        Just rest <- [stripFieldName "constraints" header],
        entry <- splitOn ',' (unwords (rest : body)),
        Just (name, flags) <- [parseConstraint entry]
      ]

    packageFlags =
      [ Map.singleton name flags
      | (header, body) <- blocks,
        Just name <- [stanzaTarget header],
        line <- body,
        Just rest <- [stripFieldName "flags" (dropWhile isSpace line)],
        let flags = Map.fromList (mapMaybe parseFlagToken (words rest)),
        not (Map.null flags)
      ]

    -- "package NAME"; the "package *" wildcard is skipped, since it would
    -- need applying to every package rather than one.
    stanzaTarget header = case words header of
      ["package", name] | name /= "*" -> Just name
      _ -> Nothing

    stripComment l = case breakOn "--" l of
      (before, _) -> before

    breakOn pat s = go "" s
      where
        go acc rest
          | pat `isPrefixOf` rest = (reverse acc, rest)
          | c : cs <- rest = go (c : acc) cs
          | otherwise = (reverse acc, "")

    stripFieldName name header
      | map toLower field == name = Just (drop 1 rest)
      | otherwise = Nothing
      where
        (field, rest) = break (== ':') header

    splitOn c s = case break (== c) s of
      (chunk, _ : more) -> chunk : splitOn c more
      (chunk, []) -> [chunk]

-- | Group a project file into top-level headers with their indented bodies.
topLevelBlocks :: [String] -> [(String, [String])]
topLevelBlocks [] = []
topLevelBlocks (l : ls)
  | continuation l = topLevelBlocks ls
  | otherwise = (l, body) : topLevelBlocks rest
  where
    (body, rest) = span continuation ls

-- | A blank line, or one indented under the header above it.
continuation :: String -> Bool
continuation x = case x of
  c : _ -> isSpace c
  [] -> True

-- | @foo +bar -baz@ from a constraints entry. Anything without a @+@ or @-@
-- token, such as a pure version constraint, yields 'Nothing'.
parseConstraint :: String -> Maybe (String, Map FlagName Bool)
parseConstraint entry = case words entry of
  pkg : rest
    | not (Map.null flags) -> Just (stripAny pkg, flags)
    where
      flags = Map.fromList (mapMaybe parseFlagToken rest)
  _ -> Nothing
  where
    -- Freeze files qualify every package as "any.NAME".
    stripAny p = if "any." `isPrefixOf` p then drop 4 p else p

parseFlagToken :: String -> Maybe (FlagName, Bool)
parseFlagToken tok = case tok of
  '+' : name | ok name -> Just (mkFlagName name, True)
  '-' : name | ok name -> Just (mkFlagName name, False)
  _ -> Nothing
  where
    ok name = not (null name) && all (\c -> isAlphaNum c || c `elem` "-_") name

mkPragma :: String -> String
mkPragma s = "{-# LANGUAGE " <> s <> " #-}"

-- | @-DNAME@ or @-DNAME=VALUE@. A bare @-DNAME@ defines it as @1@, matching
-- what the C preprocessor does.
parseDefine :: String -> Maybe (String, String)
parseDefine opt
  | "-D" `isPrefixOf` opt, not (null name) = Just (name, value)
  | otherwise = Nothing
  where
    (name, rest) = break (== '=') (drop 2 opt)
    value = case rest of
      '=' : v -> v
      _ -> "1"

-- | Every 'BuildInfo' in the package, taking every conditional branch: the
-- bundler cannot know which flags/OS the user builds with, so defaults are
-- unioned over all of them (additive, per spec).
allBuildInfos :: GenericPackageDescription -> [BuildInfo]
allBuildInfos gpd =
  concat
    [ map libBuildInfo (flattenAll (maybeToList (condLibrary gpd))),
      map libBuildInfo (flattenAll (map snd (condSubLibraries gpd))),
      map buildInfo (flattenAll (map snd (condExecutables gpd))),
      map testBuildInfo (flattenAll (map snd (condTestSuites gpd))),
      map benchmarkBuildInfo (flattenAll (map snd (condBenchmarks gpd)))
    ]
  where
    flattenAll = concatMap flattenCondTree

-- | Every 'BuildInfo' that a plain @cabal build@ of this package would use:
-- the conditionals are actually evaluated rather than unioned, so a
-- @cpp-options@ under @if flag(debug)@ is included exactly when that flag
-- defaults to on.
resolvedBuildInfos :: Map FlagName Bool -> GenericPackageDescription -> [BuildInfo]
resolvedBuildInfos imposed gpd =
  concat
    [ map libBuildInfo (resolve (maybeToList (condLibrary gpd))),
      map libBuildInfo (resolve (map snd (condSubLibraries gpd))),
      map buildInfo (resolve (map snd (condExecutables gpd))),
      map testBuildInfo (resolve (map snd (condTestSuites gpd))),
      map benchmarkBuildInfo (resolve (map snd (condBenchmarks gpd)))
    ]
  where
    resolve = concatMap (resolveCondTree flags)
    -- A project file's assignment beats the package's declared default.
    flags = Map.union imposed declared
    declared = Map.fromList [(flagName f, flagDefault f) | f <- genPackageFlags gpd]

-- | Flatten a condition tree, following the branch each condition selects.
-- An undecidable condition (an unknown compiler, say) contributes both
-- branches, so nothing is silently lost.
resolveCondTree :: Map FlagName Bool -> CondTree ConfVar c a -> [a]
resolveCondTree flags (CondNode a _ branches) = a : concatMap pick branches
  where
    pick (CondBranch cond ifTrue mIfFalse) = case evalCondition flags cond of
      Just True -> resolveCondTree flags ifTrue
      Just False -> orNothing mIfFalse
      Nothing -> resolveCondTree flags ifTrue <> orNothing mIfFalse
    orNothing = maybe [] (resolveCondTree flags)

-- | 'Nothing' when the condition depends on something we cannot pin down.
evalCondition :: Map FlagName Bool -> Condition ConfVar -> Maybe Bool
evalCondition flags = go
  where
    go c = case c of
      Lit b -> Just b
      CNot x -> not <$> go x
      -- Short-circuit, so a decidable half can still settle the whole.
      CAnd x y -> case (go x, go y) of
        (Just False, _) -> Just False
        (_, Just False) -> Just False
        (Just True, Just True) -> Just True
        _ -> Nothing
      COr x y -> case (go x, go y) of
        (Just True, _) -> Just True
        (_, Just True) -> Just True
        (Just False, Just False) -> Just False
        _ -> Nothing
      Var v -> var v
    var v = case v of
      OS os -> Just (os == buildOS)
      Arch arch -> Just (arch == buildArch)
      PackageFlag f -> Map.lookup f flags
      -- Match the compiler version the CPP evaluation claims to be.
      Impl GHC range -> Just (withinRange assumedGhcVersion range)
      Impl _ _ -> Nothing

-- | The GHC version implied by the @__GLASGOW_HASKELL__@ that
-- "Bundler.Parse" defines when evaluating CPP.
assumedGhcVersion :: Version
assumedGhcVersion = mkVersion [9, 12]

flattenCondTree :: CondTree v c a -> [a]
flattenCondTree (CondNode a _ branches) = a : concatMap flattenBranch branches
  where
    flattenBranch (CondBranch _ true mfalse) =
      flattenCondTree true <> maybe [] flattenCondTree mfalse
