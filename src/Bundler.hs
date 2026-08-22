module Bundler
  ( bundle,
  )
where

import Bundler.Cabal
import Bundler.Config
import Bundler.Discovery
import Bundler.Error
import Bundler.Format
import Bundler.Minify
import Bundler.Parse
import Bundler.ReExport
import Bundler.Rename.Apply
import Bundler.Rename.Plan
import Bundler.RenameCmd
import Bundler.Render
import Bundler.Shake
import Bundler.SourcePatch (Patch, applyPatches)
import Bundler.Symbols
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), catchE, runExceptT, throwE)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Char (isAlpha, isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (dropWhileEnd, intercalate, intersect, isInfixOf, isSuffixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Driver.Session (DynFlags)
import GHC.Hs (hsmodDecls, hsmodImports, ideclName)
import GHC.Hs qualified
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (SrcSpan (..), srcSpanEndLine, srcSpanStartLine, unLoc)
import Language.Haskell.Syntax.Module.Name (ModuleName, mkModuleName, moduleNameString)
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (hClose, hPutStr, hPutStrLn, openTempFile, readFile', stderr)
import System.Process.Typed (byteStringInput, readProcess, setStdin, shell)

-- | Run the whole pipeline, producing the bundled source for stdout.
--
-- Current stage (M1): discover local modules transitively, in dependency
-- order, and concatenate them (no renaming yet); local imports are dropped,
-- everything else is deduplicated verbatim.
bundle :: Config -> IO (Either BundleError String)
bundle cfg = runExceptT $ do
  -- Flag assignments from the build plan (cabal.project and the freeze and
  -- local files layered on it) govern every package, the user's and the
  -- libraries' alike, so they are read once from the input's project.
  projFlags <- liftIO (findProjectFlags (takeDirectory (cfgInput cfg)))
  userDefaults <- ExceptT (findProjectDefaults projFlags (takeDirectory (cfgInput cfg)))
  userFlags <- ExceptT (applyPragmaLines baseDynFlags (pdPragmas userDefaults))
  src <- liftIO (readFile' (cfgInput cfg))
  -- The bundle is one file compiled inside the user's project, so the macros
  -- GHC will have there are the ones the library's own directives must be
  -- evaluated under. A library project's cpp-options only fill in macros the
  -- user's project says nothing about, and -D overrides both.
  let cliDefines = cfgDefines cfg
      compileDefines = cliDefines <> pdDefines userDefaults
  userFile <- ExceptT (parseUserFile compileDefines userFlags (cfgInput cfg) src)
  libDirs <- traverse (dirDefaults projFlags) (cfgLibDirs cfg)
  locals <-
    ExceptT
      (discoverLocalModules compileDefines [(d, flags, pdDefines defs) | (d, flags, defs) <- libDirs] userFile)
  let withSyms0 = [(lm, moduleSymbols (lmParsed lm)) | lm <- locals]
  withSyms <- ExceptT (pure (resolveReExports withSyms0))
  let userSyms = moduleSymbols userFile
      symsOf = Map.fromList [(lmName lm, syms) | (lm, syms) <- withSyms]
  -- Where every written name comes from, which both the reachability graph
  -- and the naming below are read off. Reachability is decided before
  -- renaming, so the names of the declarations that go never take part in
  -- the bundle's flat namespace.
  inputs <- ExceptT (pure (bundleInputs symsOf userFile userSyms withSyms))
  let live = shakeLive (cfgTreeShake cfg) userFile userSyms withSyms inputs
      liveDeclsOf file decls = keepLive (lfDecls file) decls
      shakenSyms file syms = syms {msAll = Map.restrictKeys (msAll syms) (lfKeys file)}
      -- An open import does not say what it brings in, but a name the
      -- surviving code writes and no local module provides can only be
      -- coming from one, so no local name may keep that spelling.
      written =
        writtenNames
          [si {siDecls = liveDeclsOf (liveFile live (siFile si)) (siDecls si)} | si <- inputs]
  mrenamer <- liftIO (traverse startRenamer (cfgRenameCmd cfg))
  plan <-
    ExceptT
      ( mkRenamePlan
          mrenamer
          userFile
          (shakenSyms (lsUser live) userSyms)
          (wnExternal written)
          (wnQualified written)
          [(lm, shakenSyms (liveLocal live (lmName lm)) syms) | (lm, syms) <- withSyms]
      )
  libEnvs0 <-
    traverse
      (\lm -> ExceptT (pure (mkResolveEnv plan symsOf (Just (lmName lm)) (lmParsed lm))))
      locals
  userEnv <- ExceptT (pure (mkResolveEnv plan symsOf Nothing userFile))
  -- A module tree shaking emptied contributes nothing to the bundle: no
  -- banner, no pragmas, and none of its external imports.
  let liveLocals =
        [ (lm, decls, remapDirectives idxs (pfDirectives (lmParsed lm)), env)
        | (lm, env) <- zip locals libEnvs0,
          let file = liveLocal live (lmName lm),
          let idxs = lfDecls file,
          let decls = liveDeclsOf file (declsOf (lmParsed lm)),
          not (null decls)
        ]
      canonicalExts =
        Set.toAscList . Set.fromList . concat $
          [Map.elems (reQualExt e) <> Map.elems (reUnqualExt e) | (_, _, _, e) <- liveLocals]
  extAliases <- traverse (queryExtAlias mrenamer) canonicalExts
  closeRenamer mrenamer
  let extAliasMap = Map.fromList extAliases
  renamedLocals <-
    sequence
      [ ExceptT (pure ((,,) lm dirs <$> applyRenames plan symsOf env {reExtAlias = extAliasMap} decls))
      | (lm, decls, dirs, env) <- liveLocals
      ]
  let userDecls = liveDeclsOf (lsUser live) (declsOf userFile)
      userDirectives = remapDirectives (lfDecls (lsUser live)) (pfDirectives userFile)
      droppedUser = droppedUserLines userFile (lfDecls (lsUser live))
  (renamedUser, userPatches) <-
    ExceptT (pure (applyRenamesPatched plan symsOf userEnv userDecls))
  -- The user's own section is carried as original source text with the
  -- renames spliced in, so comments and formatting survive; when the
  -- patches cannot be applied cleanly, fall back to pretty-printing.
  let userSlice = sliceUserRegion userFile droppedUser userPatches
  case (userSlice, userDecls) of
    (Nothing, _ : _) ->
      liftIO . hPutStrLn stderr $
        "note: user code could not be carried verbatim; comments are dropped"
    _ -> pure ()
  -- Every name that kept its original spelling is hidden from the open
  -- imports the bundle carries. An open import may well export it, and
  -- ambiguity would be an error at every use; hiding it changes nothing,
  -- because a name is only kept when nothing in the bundle writes it with
  -- an external meaning.
  let hidden = hiddenFromOpenImports plan (shakenSyms (lsUser live) userSyms) written
      keptOpen =
        nubOrd
          [ withHiding hidden (renderImport imp)
          | (_, _, _, e) <- liveLocals,
            imp <- reOpenExtImports e
          ]
      -- An explicit import of Prelude - qualified or not - cancels the
      -- implicit one for the whole merged module. When a library's rewritten
      -- references force a canonical Prelude import and the user's file has
      -- no Prelude import of its own to govern the unqualified scope, import
      -- it unqualified (which grants qualified access too) so the user's
      -- code keeps the implicit Prelude.
      userImportsPrelude =
        any
          ((== mkModuleName "Prelude") . unLoc . ideclName . unLoc)
          (hsmodImports (unLoc (pfModule userFile)))
      extImportLine m alias =
        prefix
          <> moduleNameString m
          <> (if alias == m then "" else " as " <> moduleNameString alias)
        where
          prefix
            | m == mkModuleName "Prelude" && not userImportsPrelude = "import "
            | otherwise = "import qualified "
      extImportLines =
        [extImportLine m alias | (m, alias) <- extAliases] <> keptOpen
      out =
        assemble
          (cfgEmbedPosition cfg)
          (cfgMinify cfg)
          userDefaults
          [defs | (_, _, defs) <- libDirs]
          userFile
          (Set.fromList (map lmName locals))
          hidden
          extImportLines
          userSlice
          userDirectives
          renamedUser
          renamedLocals
  case keptOpen of
    [] -> pure ()
    kept ->
      liftIO . hPutStrLn stderr $
        "note: kept library imports whose names cannot be attributed:\n"
          <> unlines (map ("  " <>) kept)
          <> "every name the bundle keeps spelled as written is hidden from them,\n"
          <> "but a data constructor cannot be hidden and may still be ambiguous"
  checked <- ExceptT (selfCheck cliDefines out)
  let mopts = cfgMinify cfg
      -- Pre-formatting only matters for sections that stay verbatim.
      allCodeMinified = moLib mopts && moUser mopts && moImports mopts
      formatStage = case cfgFormat cfg of
        _ | allCodeMinified -> pure checked
        FormatNone -> pure checked
        FormatBuiltin -> case formatBuiltin checked of
          Right formatted -> reparseAs "hindent" formatted
          -- The default formatter must never make bundling fail: warn and
          -- fall back to the raw (already self-checked) output.
          Left err -> do
            liftIO . hPutStrLn stderr $
              "warning: builtin hindent could not format the bundle"
                <> " (emitting unformatted output): "
                <> err
            pure checked
        FormatCmd cmd -> do
          formatted <- ExceptT (runFormatter cmd checked)
          reparseAs cmd formatted
      minifyStage formatted
        | anyMinify mopts = do
            minified <- ExceptT (minifyWith mopts formatted)
            reparseAs "--minify" minified
        | otherwise = pure formatted
  -- A formatting failure must not lose the (already parse-checked)
  -- bundle: save it and tell the user where it went.
  fmap stripUserBanner $
    (formatStage >>= minifyStage) `catchE` \err -> do
      path <- liftIO (saveUnformatted checked)
      liftIO (hPutStrLn stderr ("note: the unformatted bundle was saved to " <> path))
      throwE err
  where
    -- Formatters are arbitrary; make sure the result is still Haskell.
    reparseAs cmd formatted = do
      reparsed <- liftIO (parseHaskellFile (cfgDefines cfg) baseDynFlags "<formatted output>" formatted)
      case reparsed of
        Left err ->
          ExceptT . pure . Left $
            FormatCmdError cmd ("output no longer parses:\n" <> renderBundleError err)
        Right _ -> pure formatted

    dirDefaults :: ProjectFlags -> FilePath -> ExceptT BundleError IO (FilePath, DynFlags, ProjectDefaults)
    dirDefaults projFlags dir = do
      defs <- ExceptT (findProjectDefaults projFlags dir)
      flags <- ExceptT (applyPragmaLines baseDynFlags (pdPragmas defs))
      pure (dir, flags, defs)

    -- The canonical qualifier for one external module: the module name
    -- itself, unless the rename command's extmod kind says otherwise.
    queryExtAlias mrenamer m = case mrenamer of
      Nothing -> pure (m, m)
      Just renamer -> do
        alias <-
          ExceptT . liftIO . queryRenamer renamer $
            RenameQuery
              { rqKind = "extmod",
                rqModule = moduleNameString m,
                rqName = moduleNameString m,
                rqSuffix = filter (/= '.') (moduleNameString m)
              }
        pure (m, mkModuleName alias)

    closeRenamer = maybe (pure ()) (ExceptT . liftIO . stopRenamer)

declsOf :: ParsedFile -> [GHC.Hs.LHsDecl GHC.Hs.GhcPs]
declsOf pf = hsmodDecls (unLoc (pfModule pf))

-- | The user-code banner is an internal marker (the granular minifier
-- tells sections apart by it); it has no place in the final output. One
-- adjacent blank line goes with it so chunk spacing stays single.
stripUserBanner :: String -> String
stripUserBanner = unlines . go . lines
  where
    go (l : rest)
      | dropWhile (== ' ') l == "-- ### (user code)" = go (dropOneBlank rest)
      | otherwise = l : go rest
    go [] = []
    dropOneBlank ("" : rest) = rest
    dropOneBlank rest = rest

-- | The user file's own code as original text with the rename patches
-- applied: everything from the first top-level declaration (or the first
-- preserved directive below the header/import section) to the end of the
-- file. Directives above that point, and comment lines sitting between
-- imports (the imports themselves are rebuilt), are prepended in their
-- original order. 'Nothing' when there is nothing to slice or a patch
-- cannot be applied cleanly.
sliceUserRegion :: ParsedFile -> IntSet -> [Patch] -> Maybe String
sliceUserRegion pf dropped patches = do
  let modl = unLoc (pfModule pf)
      realEnd sp = case sp of
        RealSrcSpan r _ -> [srcSpanEndLine r]
        _ -> []
      realStart sp = case sp of
        RealSrcSpan r _ -> [srcSpanStartLine r]
        _ -> []
      afterHeader =
        1
          + maximum
            ( 0
                : concatMap (realEnd . GHC.Hs.getLocA) (hsmodImports modl)
                  <> concatMap (realEnd . GHC.Hs.getLocA) (maybe [] pure (GHC.Hs.hsmodName modl))
                  <> concatMap (realEnd . GHC.Hs.getLocA) (maybe [] pure (GHC.Hs.hsmodExports modl))
            )
      importEnds = concatMap (realEnd . GHC.Hs.getLocA) (hsmodImports modl)
      declStarts = concatMap (realStart . GHC.Hs.getLocA) (hsmodDecls modl)
      dirStarts = [line | (_, line, _) <- pfDirectives pf, line >= afterHeader]
  case declStarts <> dirStarts of
    [] -> Nothing
    _ -> Just ()
  -- With imports present the region can start right after them, keeping
  -- comments above the first declaration. Without any there is no reliable
  -- lower bound (a header's `where` may sit on its own line), so start at
  -- the first declaration.
  let start
        | not (null importEnds) = 1 + maximum importEnds
        | otherwise = max afterHeader (minimum (declStarts <> dirStarts))
  patched <- applyPatches patches (pfSource pf)
  let patchedLines = lines patched
      region =
        map snd . dropWhileEnd (null . snd) . dropWhile (null . snd) $
          [ (n, l)
          | (n, l) <- zip [1 ..] patchedLines,
            n >= start,
            n `IntSet.notMember` dropped
          ]
      importSpans =
        [ (srcSpanStartLine r, srcSpanEndLine r)
        | i <- hsmodImports modl,
          RealSrcSpan r _ <- [GHC.Hs.getLocA i]
        ]
      directiveLines = Set.fromList [line | (_, line, _) <- pfDirectives pf]
      -- Comment lines between imports: within the import section, not
      -- covered by any import's span, not blank, and not a preserved
      -- directive (those are re-emitted below).
      importComments
        | null importSpans = []
        | otherwise =
            [ (n, l)
            | (n, l) <- zip [1 ..] patchedLines,
              n >= minimum (map fst importSpans),
              n < start,
              not (any (\(s, e) -> n >= s && n <= e) importSpans),
              n `Set.notMember` directiveLines,
              not (null (dropWhile (== ' ') l))
            ]
      preDirs = [(line, text) | (_, line, text) <- pfDirectives pf, line < start]
      pre = map snd (sortOn fst (importComments <> preDirs))
      -- Removing a declaration leaves its blank line behind, next to the
      -- one the declaration before it already had.
      body
        | IntSet.null dropped = region
        | otherwise = squeezeBlanks region
  pure (intercalate "\n" (pre <> body))

-- | The names that keep their original spelling in the bundle and that
-- something in it writes, rendered as import-list items.
--
-- Only written names can go ambiguous, and a data constructor is left out
-- because an import list cannot name one on its own (see the README).
hiddenFromOpenImports :: RenamePlan -> ModuleSymbols -> WrittenNames -> [String]
hiddenFromOpenImports plan userSyms written =
  nubOrd . sortOn id $
    [ item name
    | (key@(ns, old), name) <- kept,
      name == old,
      ns /= NsData,
      key `Set.member` wnLocal written,
      key `Set.notMember` wnExternal written
    ]
  where
    -- The user's own names are never renamed, so a library's open import
    -- can shadow one of those just as easily.
    kept =
      [(key, new) | entries <- Map.elems (rpByModule plan), (key, new) <- Map.toList entries]
        <> [(key, name) | key@(_, name) <- Map.keys (msAll userSyms)]
    item name
      | isOperatorString name = "(" <> name <> ")"
      | otherwise = name

-- | Add names to an import's hiding list, opening one if it has none. The
-- rendered import is normalized to a single line first, so that the closing
-- parenthesis of an existing list is where this expects it.
withHiding :: [String] -> String -> String
withHiding [] rendered = rendered
withHiding names rendered
  | " hiding (" `isInfixOf` line,
    Just core <- stripSuffix ")" line =
      case reverse core of
        '(' : _ -> core <> list <> ")"
        _ -> core <> ", " <> list <> ")"
  | otherwise = line <> " hiding (" <> list <> ")"
  where
    line = unwords (words rendered)
    list = intercalate ", " names
    stripSuffix suffix s
      | suffix `isSuffixOf` s = Just (take (length s - length suffix) s)
      | otherwise = Nothing

-- | Collapse runs of blank lines into one.
squeezeBlanks :: [String] -> [String]
squeezeBlanks ("" : rest@("" : _)) = squeezeBlanks rest
squeezeBlanks (l : rest) = l : squeezeBlanks rest
squeezeBlanks [] = []

-- | The source lines of the user's declarations that tree shaking dropped,
-- each together with the comment block written directly above it.
droppedUserLines :: ParsedFile -> IntSet -> IntSet
droppedUserLines pf liveDecls =
  -- Two declarations can share a line (@a = 1; b = 2@), and the surviving
  -- one keeps it.
  IntSet.fromList (concatMap range gone) `IntSet.difference` keptLines
  where
    srcLines = lines (pfSource pf)
    spans keep =
      [ real
      | (i, d) <- zip [0 ..] (declsOf pf),
        keep (i `IntSet.member` liveDecls),
        RealSrcSpan real _ <- [GHC.Hs.getLocA d]
      ]
    gone = spans not
    keptLines =
      IntSet.fromList
        [ line
        | real <- spans id,
          line <- [srcSpanStartLine real .. srcSpanEndLine real]
        ]
    range real = [commentStart (srcSpanStartLine real) .. srcSpanEndLine real]
    commentStart n
      | n > 1,
        Just l <- lookup (n - 1) (zip [1 ..] srcLines),
        isAttached l =
          commentStart (n - 1)
      | otherwise = n
    isAttached l = case dropWhile (== ' ') l of
      '-' : '-' : _ -> True
      '{' : '-' : '#' : _ -> True
      _ -> False

-- | Keep the declarations tree shaking marked live, by index.
keepLive :: IntSet -> [a] -> [a]
keepLive live xs = [x | (i, x) <- zip [0 ..] xs, i `IntSet.member` live]

-- | Re-anchor preserved CPP directives after declarations were dropped: a
-- directive anchored to declaration @i@ moves to wherever the declarations
-- that survived before it now end.
remapDirectives :: IntSet -> [(Int, Int, String)] -> [(Int, Int, String)]
remapDirectives live directives =
  [(IntSet.size (fst (IntSet.split i live)), line, text) | (i, line, text) <- directives]

-- | Every file of the bundle paired with the environment that says where
-- the names it writes come from.
--
-- Resolving a written name only needs to know which module provides it, so
-- the plan behind these environments may name everything as it already is.
bundleInputs ::
  Map.Map ModuleName ModuleSymbols ->
  ParsedFile ->
  ModuleSymbols ->
  [(LocalModule, ModuleSymbols)] ->
  Either BundleError [ShakeInput]
bundleInputs symsOf userFile userSyms withSyms = do
  libEnvs <-
    traverse
      (\(lm, _) -> mkResolveEnv idPlan symsOf (Just (lmName lm)) (lmParsed lm))
      withSyms
  userEnv <- mkResolveEnv idPlan symsOf Nothing userFile
  pure $
    ShakeInput Nothing (declsOf userFile) userSyms userEnv
      : [ ShakeInput (Just (lmName lm)) (declsOf (lmParsed lm)) syms env
        | ((lm, syms), env) <- zip withSyms libEnvs
        ]
  where
    idPlan =
      RenamePlan
        ( Map.fromList
            [ (lmName lm, Map.mapWithKey (\(_, name) _ -> name) (msAll syms))
            | (lm, syms) <- withSyms
            ]
        )

-- | Decide what survives, honoring @--tree-shake-lib@ / @--tree-shake-app@.
-- With neither of them everything does.
shakeLive ::
  TreeShakeOptions ->
  ParsedFile ->
  ModuleSymbols ->
  [(LocalModule, ModuleSymbols)] ->
  [ShakeInput] ->
  LiveSet
shakeLive opts userFile userSyms withSyms inputs
  | not (anyTreeShake opts) = everything
  | otherwise = shake wholeFiles roots inputs
  where
    everything =
      keepEverything
        (length (declsOf userFile), userSyms)
        [ (lmName lm, length (declsOf (lmParsed lm)), syms)
        | (lm, syms) <- withSyms
        ]
    appRoots
      | tsApp opts = userRoots userFile userSyms
      | otherwise = Nothing
    wholeFiles =
      Set.fromList $
        [Nothing | appRoots == Nothing]
          <> [Just (lmName lm) | not (tsLib opts), (lm, _) <- withSyms]
    roots =
      Set.fromList
        [(Nothing, key) | keys <- maybe [] pure appRoots, key <- Set.toList keys]

-- | Stitch the output text together from pretty-printed pieces: pragma
-- union, user module header, merged imports, then the (renamed)
-- declarations of every local module in dependency order and finally the
-- user's own.
assemble ::
  EmbedPosition ->
  MinifyOptions ->
  ProjectDefaults ->
  [ProjectDefaults] ->
  ParsedFile ->
  -- | Every local module that was expanded, including any that tree shaking
  -- emptied: their imports are gone from the bundle all the same.
  Set.Set ModuleName ->
  -- | Names to hide from the user's own open imports.
  [String] ->
  [String] ->
  Maybe String ->
  [(Int, Int, String)] ->
  [GHC.Hs.LHsDecl GHC.Hs.GhcPs] ->
  [(LocalModule, [(Int, Int, String)], [GHC.Hs.LHsDecl GHC.Hs.GhcPs])] ->
  String
assemble embedPos minifyOpts userDefaults libDefaults userFile localNames hidden extImportLines userSlice userDirectives userDecls locals =
  intercalate "\n\n" (filter (not . null) chunks) <> "\n"
  where
    chunks =
      [ intercalate "\n" header,
        fromMaybe "" (renderModuleHeader (pfModule userFile)),
        intercalate "\n" imports
      ]
        <> bodyChunks
    -- The user chunk carries a banner too (when there is library code to
    -- distinguish it from), so sections stay identifiable in the formatted
    -- output - the granular minifier relies on these banner lines.
    userChunk
      | null locals = intercalate "\n\n" userPieces
      | otherwise = intercalate "\n\n" ("-- ### (user code)" : userPieces)
    bodyChunks = case embedPos of
      EmbedAfter -> [userChunk] <> localChunks
      EmbedBefore -> localChunks <> [userChunk]

    -- The user's declarations: the patched original text when available
    -- (directives included), otherwise pretty-printed with any preserved
    -- CPP directive lines re-emitted at the declaration boundaries they
    -- came from. A signature merges with its binding unless directives
    -- separate them.
    userPieces = case userSlice of
      Just text -> [text]
      Nothing -> declPieces userDirectives userDecls

    -- The user's header block comes first and verbatim, so its comments
    -- (and the order of its own pragmas) survive; the pragmas the bundle
    -- picks up from elsewhere are appended, minus the ones already there.
    header = userHeader <> filter (`notElem` userHeader) inheritedPragmas
    userHeader = pfHeader userFile
    inheritedPragmas =
      nubOrd . concat $
        [ pdPragmas userDefaults,
          pfPragmas userFile,
          concatMap pdPragmas libDefaults,
          concatMap (\(lm, _, _) -> pfPragmas (lmParsed lm)) locals
        ]

    -- The user's imports survive verbatim (minus expanded local modules);
    -- library imports arrive pre-digested as canonical/kept lines.
    userImports =
      [ withHiding (if openExternal (unLoc imp) then hidden else []) (renderImport imp)
      | imp <- hsmodImports (unLoc (pfModule userFile)),
        unLoc (ideclName (unLoc imp)) `Set.notMember` localNames
      ]
    -- An import that puts names in unqualified scope without saying which
    -- ones: no list at all, or a hiding list.
    openExternal imp =
      GHC.Hs.ideclQualified imp == GHC.Hs.NotQualified
        && case GHC.Hs.ideclImportList imp of
          Nothing -> True
          Just (GHC.Hs.EverythingBut, _) -> True
          _ -> False
    imports = nubOrd (userImports <> extImportLines)

    -- Minifying the library section collapses each run of consecutive
    -- declarations onto one line, and a preserved conditional breaks the
    -- run: the code before it and the code after it land on separate lines.
    -- Top-level order carries no meaning in Haskell, so the conditionals go
    -- last and everything else joins up into a single line. Unminified
    -- output keeps every declaration where it was written.
    localChunks
      | moLib minifyOpts =
          [chunkFor lm [p | (False, p) <- ps] | (lm, ps) <- pieced]
            <> [ intercalate "\n\n" conds
               | let conds = [p | (_, ps) <- pieced, (True, p) <- ps],
                 not (null conds)
               ]
      | otherwise = [chunkFor lm (map snd ps) | (lm, ps) <- pieced]
      where
        pieced =
          [ (lm, taggedDeclPieces dirs decls)
          | (lm, dirs, decls) <- locals
          ]
        chunkFor lm ps =
          intercalate "\n\n" (("-- ### " <> moduleNameString (lmName lm)) : ps)

-- | Render declarations, joining each type/pattern-synonym signature with
-- the binding that follows it (GHC parses them as separate declarations,
-- but a blank line between @f :: ...@ and @f = ...@ reads as noise), and
-- re-emitting any preserved CPP directive lines at the declaration
-- boundaries they came from. A signature merges with its binding unless a
-- directive separates them.
declPieces :: [(Int, Int, String)] -> [GHC.Hs.LHsDecl GHC.Hs.GhcPs] -> [String]
declPieces directives decls = map snd (taggedDeclPieces directives decls)

-- | 'declPieces', with each piece marked as to whether a preserved
-- conditional encloses it. Directive lines themselves count as enclosed, so
-- filtering on the mark keeps every conditional block intact and in order.
taggedDeclPieces ::
  [(Int, Int, String)] ->
  [GHC.Hs.LHsDecl GHC.Hs.GhcPs] ->
  [(Bool, String)]
taggedDeclPieces directives = go 0 0
  where
    go depth i [] = fst (dirPieces depth i)
    go depth i (d : ds) =
      pieces <> case ds of
        d2 : rest
          | signatureFor d d2,
            null (directivesAt (i + 1)) ->
              (depth' > 0, renderDecl d <> "\n" <> renderDecl d2) : go depth' (i + 2) rest
        _ -> (depth' > 0, renderDecl d) : go depth' (i + 1) ds
      where
        (pieces, depth') = dirPieces depth i

    dirPieces depth i = case pruneEmptyConditionals (directivesAt i) of
      [] -> ([], depth)
      ts -> ([(True, intercalate "\n" ts)], depth + sum (map nesting ts))

    directivesAt i = [text | (j, _, text) <- directives, j == i]

    nesting l = case takeWhile isAlpha (dropWhile isSpace (drop 1 l)) of
      kw | kw `elem` ["if", "ifdef", "ifndef"] -> 1 :: Int
      "endif" -> -1
      _ -> 0

-- | Drop conditionals left enclosing nothing, which happens when everything
-- between them was an import or a header pragma and got hoisted into the
-- bundle's own import or pragma block. Only a matched group with nothing in
-- between goes: an @#endif@ closing the conditional of an earlier
-- declaration sits in the same group and must stay.
pruneEmptyConditionals :: [String] -> [String]
pruneEmptyConditionals = go
  where
    go ts = case break isOpener ts of
      (before, opener : rest) ->
        let (middles, tailer) = span isElseLike rest
         in case tailer of
              closer : after
                | isEndif closer -> go (before <> after)
              _ -> before <> (opener : middles <> tailer)
      (before, []) -> before
    keyword l = takeWhile isAlpha (dropWhile isSpace (drop 1 l))
    isOpener l = keyword l `elem` ["if", "ifdef", "ifndef"]
    isElseLike l = keyword l `elem` ["else", "elif"]
    isEndif l = keyword l == "endif"

-- | Does the first declaration declare a signature for something the second
-- one binds?
signatureFor :: GHC.Hs.LHsDecl GHC.Hs.GhcPs -> GHC.Hs.LHsDecl GHC.Hs.GhcPs -> Bool
signatureFor sig bind = case (unLoc sig, unLoc bind) of
  (GHC.Hs.SigD _ s, GHC.Hs.ValD _ b) ->
    not (null (sigNames s `intersect` bindNames b))
  _ -> False
  where
    sigNames s = case s of
      GHC.Hs.TypeSig _ ns _ -> map (nameOf . unLoc) ns
      GHC.Hs.PatSynSig _ ns _ -> map (nameOf . unLoc) ns
      _ -> []
    bindNames b = case b of
      GHC.Hs.FunBind {GHC.Hs.fun_id = n} -> [nameOf (unLoc n)]
      GHC.Hs.PatBind {GHC.Hs.pat_lhs = p} ->
        map nameOf (GHC.Hs.collectPatBinders GHC.Hs.CollNoDictBinders p)
      GHC.Hs.PatSynBind _ (GHC.Hs.PSB {GHC.Hs.psb_id = n}) -> [nameOf (unLoc n)]
      _ -> []
    nameOf = occNameString . rdrNameOcc

-- | Save the pre-formatting bundle for the user to salvage after a
-- formatter failure.
saveUnformatted :: String -> IO FilePath
saveUnformatted contents = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp "bundler-hs.hs"
  hPutStr h contents
  hClose h
  pure path

-- | Pipe the bundle through the user's formatter (stdin to stdout).
runFormatter :: String -> String -> IO (Either BundleError String)
runFormatter cmd input = do
  (code, out, err) <-
    readProcess
      (setStdin (byteStringInput (LBS8.pack input)) (shell cmd))
  pure $ case code of
    ExitSuccess -> Right (LBS8.unpack out)
    ExitFailure n ->
      Left
        ( FormatCmdError
            cmd
            ("exited with code " <> show n <> ":\n" <> LBS8.unpack err)
        )

-- | Re-parse our own output before emitting it: the pretty-printer is not
-- guaranteed to produce re-parseable code in every corner case, and a bundle
-- that does not even parse must never reach stdout silently.
selfCheck :: [(String, String)] -> String -> IO (Either BundleError String)
selfCheck defs out = do
  reparsed <- parseHaskellFile defs baseDynFlags "<bundled output>" out
  pure $ case reparsed of
    Left err -> Left (SelfCheckError (renderBundleError err) out)
    Right _ -> Right out
