module Bundler
  ( bundle
  ) where

import Bundler.Cabal
import Bundler.Config
import Bundler.Discovery
import Bundler.Error
import Bundler.Parse
import Bundler.Render
import Bundler.Rename.Apply
import Bundler.Rename.Plan
import Bundler.Symbols
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Driver.Session (DynFlags)
import GHC.Hs (hsmodDecls, hsmodImports, ideclName)
import GHC.Hs qualified
import GHC.Types.SrcLoc (unLoc)
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import System.FilePath (takeDirectory)
import System.IO (readFile')

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
  userFile <- ExceptT (parseHaskellFile userFlags (cfgInput cfg) src)
  srcDirs <- traverse dirDefaults (cfgSrcDirs cfg)
  locals <- ExceptT (discoverLocalModules [(d, flags) | (d, flags, _) <- srcDirs] userFile)
  let withSyms = [(lm, moduleSymbols (lmParsed lm)) | lm <- locals]
  plan <- ExceptT (pure (mkRenamePlan userFile (moduleSymbols userFile) withSyms))
  let symsOf = Map.fromList [(lmName lm, syms) | (lm, syms) <- withSyms]
      renameIn self pf = do
        env <- mkResolveEnv plan symsOf self pf
        applyRenames plan env (declsOf pf)
  renamedLocals <-
    traverse
      (\lm -> ExceptT (pure ((,) lm <$> renameIn (Just (lmName lm)) (lmParsed lm))))
      locals
  renamedUser <- ExceptT (pure (renameIn Nothing userFile))
  let out =
        assemble
          userDefaults
          [defs | (_, _, defs) <- srcDirs]
          userFile
          renamedUser
          [(lm, decls) | (lm, decls) <- renamedLocals]
  ExceptT (selfCheck out)
  where
    dirDefaults :: FilePath -> ExceptT BundleError IO (FilePath, DynFlags, ProjectDefaults)
    dirDefaults dir = do
      defs <- ExceptT (findProjectDefaults dir)
      flags <- ExceptT (applyPragmaLines baseDynFlags (pdPragmas defs))
      pure (dir, flags, defs)

declsOf :: ParsedFile -> [GHC.Hs.LHsDecl GHC.Hs.GhcPs]
declsOf pf = hsmodDecls (unLoc (pfModule pf))

-- | Stitch the output text together from pretty-printed pieces: pragma
-- union, user module header, merged imports, then the (renamed)
-- declarations of every local module in dependency order and finally the
-- user's own.
assemble
  :: ProjectDefaults
  -> [ProjectDefaults]
  -> ParsedFile
  -> [GHC.Hs.LHsDecl GHC.Hs.GhcPs]
  -> [(LocalModule, [GHC.Hs.LHsDecl GHC.Hs.GhcPs])]
  -> String
assemble userDefaults libDefaults userFile userDecls locals =
  intercalate "\n\n" (filter (not . null) chunks) <> "\n"
  where
    chunks =
      [ intercalate "\n" pragmas
      , fromMaybe "" (renderModuleHeader (pfModule userFile))
      , intercalate "\n" imports
      ]
        <> map localChunk locals
        <> [intercalate "\n\n" (map renderDecl userDecls)]

    pragmas =
      nubOrd . concat $
        [ pdPragmas userDefaults
        , pfPragmas userFile
        , concatMap pdPragmas libDefaults
        , concatMap (pfPragmas . lmParsed . fst) locals
        ]

    localNames = Set.fromList (map (lmName . fst) locals)
    keptImports pf =
      [ imp
      | imp <- hsmodImports (unLoc (pfModule pf))
      , unLoc (ideclName (unLoc imp)) `Set.notMember` localNames
      ]
    imports =
      nubOrd . map renderImport . concat $
        keptImports userFile : map (keptImports . lmParsed . fst) locals

    localChunk (lm, decls) =
      intercalate "\n\n" $
        ("-- ### " <> moduleNameString (lmName lm))
          : map renderDecl decls

-- | Re-parse our own output before emitting it: the pretty-printer is not
-- guaranteed to produce re-parseable code in every corner case, and a bundle
-- that does not even parse must never reach stdout silently.
selfCheck :: String -> IO (Either BundleError String)
selfCheck out = do
  reparsed <- parseHaskellFile baseDynFlags "<bundled output>" out
  pure $ case reparsed of
    Left err -> Left (SelfCheckError (renderBundleError err) out)
    Right _ -> Right out
