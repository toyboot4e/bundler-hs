module Bundler.Cabal
  ( ProjectDefaults (..),
    findProjectDefaults,
  )
where

import Bundler.Error
import Data.ByteString qualified as BS
import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Distribution.Compiler (CompilerFlavor (GHC))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty (prettyShow)
import Distribution.System (buildArch, buildOS)
import Distribution.Types.Benchmark (benchmarkBuildInfo)
import Distribution.Types.BuildInfo (BuildInfo, cppOptions, defaultExtensions, defaultLanguage)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Executable (buildInfo)
import Distribution.Types.Flag (FlagName, flagDefault, flagName)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library (libBuildInfo)
import Distribution.Types.TestSuite (testBuildInfo)
import Distribution.Version (Version, mkVersion, withinRange)
import System.Directory (canonicalizePath, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

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

-- | Walk upward from @start@ (a directory) to the first directory containing
-- a @.cabal@ file. No project at all is fine (empty defaults); more than one
-- @.cabal@ in the same directory is an error.
findProjectDefaults :: FilePath -> IO (Either BundleError ProjectDefaults)
findProjectDefaults start = canonicalizePath start >>= go
  where
    go dir = do
      cabals <- filter ((== ".cabal") . takeExtension) <$> listDirectory dir
      case cabals of
        [name] -> readDefaults dir (dir </> name)
        [] ->
          let parent = takeDirectory dir
           in if parent == dir
                then pure (Right (ProjectDefaults start [] []))
                else go parent
        several -> pure (Left (AmbiguousCabal dir several))

readDefaults :: FilePath -> FilePath -> IO (Either BundleError ProjectDefaults)
readDefaults root path = do
  contents <- BS.readFile path
  pure $ case parseGenericPackageDescriptionMaybe contents of
    Nothing -> Left (CabalError path "not a valid package description")
    Just gpd ->
      Right
        ProjectDefaults
          { pdRoot = root,
            pdPragmas = map mkPragma (nubOrd (languages <> extensions)),
            pdDefines = nubOrd (concatMap (mapMaybe parseDefine . cppOptions) (resolvedBuildInfos gpd))
          }
      where
        infos = allBuildInfos gpd
        languages = map prettyShow (concatMap (maybeToList . defaultLanguage) infos)
        extensions = map prettyShow (concatMap defaultExtensions infos)

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
resolvedBuildInfos :: GenericPackageDescription -> [BuildInfo]
resolvedBuildInfos gpd =
  concat
    [ map libBuildInfo (resolve (maybeToList (condLibrary gpd))),
      map libBuildInfo (resolve (map snd (condSubLibraries gpd))),
      map buildInfo (resolve (map snd (condExecutables gpd))),
      map testBuildInfo (resolve (map snd (condTestSuites gpd))),
      map benchmarkBuildInfo (resolve (map snd (condBenchmarks gpd)))
    ]
  where
    resolve = concatMap (resolveCondTree flags)
    flags = Map.fromList [(flagName f, flagDefault f) | f <- genPackageFlags gpd]

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
