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
import Bundler.Symbols
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), catchE, runExceptT, throwE)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate, intersect)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Driver.Session (DynFlags)
import GHC.Hs (hsmodDecls, hsmodImports, ideclName)
import GHC.Hs qualified
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (unLoc)
import Language.Haskell.Syntax.Module.Name (mkModuleName, moduleNameString)
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
  userDefaults <- ExceptT (findProjectDefaults (takeDirectory (cfgInput cfg)))
  userFlags <- ExceptT (applyPragmaLines baseDynFlags (pdPragmas userDefaults))
  src <- liftIO (readFile' (cfgInput cfg))
  userFile <- ExceptT (parseUserFile userFlags (cfgInput cfg) src)
  srcDirs <- traverse dirDefaults (cfgSrcDirs cfg)
  locals <- ExceptT (discoverLocalModules [(d, flags) | (d, flags, _) <- srcDirs] userFile)
  let withSyms0 = [(lm, moduleSymbols (lmParsed lm)) | lm <- locals]
  withSyms <- ExceptT (pure (resolveReExports withSyms0))
  mrenamer <- liftIO (traverse startRenamer (cfgRenameCmd cfg))
  plan <- ExceptT (mkRenamePlan mrenamer userFile (moduleSymbols userFile) withSyms)
  let symsOf = Map.fromList [(lmName lm, syms) | (lm, syms) <- withSyms]
      renameWith env pf = applyRenames plan symsOf env (declsOf pf)
  libEnvs0 <-
    traverse
      (\lm -> ExceptT (pure (mkResolveEnv plan symsOf (Just (lmName lm)) (lmParsed lm))))
      locals
  userEnv <- ExceptT (pure (mkResolveEnv plan symsOf Nothing userFile))
  let canonicalExts =
        Set.toAscList . Set.fromList . concat $
          [Map.elems (reQualExt e) <> Map.elems (reUnqualExt e) | e <- libEnvs0]
  extAliases <- traverse (queryExtAlias mrenamer) canonicalExts
  closeRenamer mrenamer
  let extAliasMap = Map.fromList extAliases
      libEnvs = [e {reExtAlias = extAliasMap} | e <- libEnvs0]
  renamedLocals <-
    sequence
      [ ExceptT (pure ((,) lm <$> renameWith env (lmParsed lm)))
      | (lm, env) <- zip locals libEnvs
      ]
  renamedUser <- ExceptT (pure (renameWith userEnv userFile))
  let keptOpen = nubOrd (map renderImport (concatMap reOpenExtImports libEnvs))
      extImportLines =
        [ "import qualified "
            <> moduleNameString m
            <> (if alias == m then "" else " as " <> moduleNameString alias)
        | (m, alias) <- extAliases
        ]
          <> keptOpen
      out =
        assemble
          (cfgEmbedPosition cfg)
          userDefaults
          [defs | (_, _, defs) <- srcDirs]
          userFile
          extImportLines
          renamedUser
          renamedLocals
  case keptOpen of
    [] -> pure ()
    kept ->
      liftIO . hPutStrLn stderr $
        "note: kept library imports whose names cannot be attributed:\n"
          <> unlines (map ("  " <>) kept)
          <> "these may make names ambiguous in the bundle"
  checked <- ExceptT (selfCheck out)
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
  (formatStage >>= minifyStage) `catchE` \err -> do
    path <- liftIO (saveUnformatted checked)
    liftIO (hPutStrLn stderr ("note: the unformatted bundle was saved to " <> path))
    throwE err
  where
    -- Formatters are arbitrary; make sure the result is still Haskell.
    reparseAs cmd formatted = do
      reparsed <- liftIO (parseHaskellFile baseDynFlags "<formatted output>" formatted)
      case reparsed of
        Left err ->
          ExceptT . pure . Left $
            FormatCmdError cmd ("output no longer parses:\n" <> renderBundleError err)
        Right _ -> pure formatted

    dirDefaults :: FilePath -> ExceptT BundleError IO (FilePath, DynFlags, ProjectDefaults)
    dirDefaults dir = do
      defs <- ExceptT (findProjectDefaults dir)
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
                rqSuffix = filter (/= '.') (moduleNameString m),
                rqName = moduleNameString m
              }
        pure (m, mkModuleName alias)

    closeRenamer = maybe (pure ()) (ExceptT . liftIO . stopRenamer)

declsOf :: ParsedFile -> [GHC.Hs.LHsDecl GHC.Hs.GhcPs]
declsOf pf = hsmodDecls (unLoc (pfModule pf))

-- | Stitch the output text together from pretty-printed pieces: pragma
-- union, user module header, merged imports, then the (renamed)
-- declarations of every local module in dependency order and finally the
-- user's own.
assemble ::
  EmbedPosition ->
  ProjectDefaults ->
  [ProjectDefaults] ->
  ParsedFile ->
  [String] ->
  [GHC.Hs.LHsDecl GHC.Hs.GhcPs] ->
  [(LocalModule, [GHC.Hs.LHsDecl GHC.Hs.GhcPs])] ->
  String
assemble embedPos userDefaults libDefaults userFile extImportLines userDecls locals =
  intercalate "\n\n" (filter (not . null) chunks) <> "\n"
  where
    chunks =
      [ intercalate "\n" pragmas,
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
      EmbedAfter -> [userChunk] <> map localChunk locals
      EmbedBefore -> map localChunk locals <> [userChunk]

    -- The user's declarations, with any preserved CPP directive lines
    -- re-emitted at the declaration boundaries they came from. A signature
    -- merges with its binding unless directives separate them.
    userPieces = go (0 :: Int) userDecls
      where
        go i [] = dirPieces i
        go i (d : ds) =
          dirPieces i <> case ds of
            d2 : rest
              | signatureFor d d2,
                null (directivesAt (i + 1)) ->
                  (renderDecl d <> "\n" <> renderDecl d2) : go (i + 2) rest
            _ -> renderDecl d : go (i + 1) ds
        dirPieces i = case directivesAt i of
          [] -> []
          ts -> [intercalate "\n" ts]
    directivesAt i = [text | (j, text) <- pfDirectives userFile, j == i]

    pragmas =
      nubOrd . concat $
        [ pdPragmas userDefaults,
          pfPragmas userFile,
          concatMap pdPragmas libDefaults,
          concatMap (pfPragmas . lmParsed . fst) locals
        ]

    localNames = Set.fromList (map (lmName . fst) locals)
    -- The user's imports survive verbatim (minus expanded local modules);
    -- library imports arrive pre-digested as canonical/kept lines.
    userImports =
      [ renderImport imp
      | imp <- hsmodImports (unLoc (pfModule userFile)),
        unLoc (ideclName (unLoc imp)) `Set.notMember` localNames
      ]
    imports = nubOrd (userImports <> extImportLines)

    localChunk (lm, decls) =
      intercalate "\n\n" $
        ("-- ### " <> moduleNameString (lmName lm))
          : mergeSigs decls

-- | Render declarations, joining each type/pattern-synonym signature with
-- the binding that follows it (GHC parses them as separate declarations,
-- but a blank line between @f :: ...@ and @f = ...@ reads as noise).
mergeSigs :: [GHC.Hs.LHsDecl GHC.Hs.GhcPs] -> [String]
mergeSigs (d : d2 : rest)
  | signatureFor d d2 = (renderDecl d <> "\n" <> renderDecl d2) : mergeSigs rest
mergeSigs (d : rest) = renderDecl d : mergeSigs rest
mergeSigs [] = []

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
selfCheck :: String -> IO (Either BundleError String)
selfCheck out = do
  reparsed <- parseHaskellFile baseDynFlags "<bundled output>" out
  pure $ case reparsed of
    Left err -> Left (SelfCheckError (renderBundleError err) out)
    Right _ -> Right out
