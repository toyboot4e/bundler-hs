module Bundler
  ( bundle
  ) where

import Bundler.Cabal
import Bundler.Config
import Bundler.Discovery
import Bundler.Error
import Bundler.Parse
import Bundler.Render
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Driver.Session (DynFlags)
import GHC.Hs (hsmodDecls, hsmodImports, ideclName)
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
  let out = assemble userDefaults [defs | (_, _, defs) <- srcDirs] userFile locals
  ExceptT (selfCheck out)
  where
    dirDefaults :: FilePath -> ExceptT BundleError IO (FilePath, DynFlags, ProjectDefaults)
    dirDefaults dir = do
      defs <- ExceptT (findProjectDefaults dir)
      flags <- ExceptT (applyPragmaLines baseDynFlags (pdPragmas defs))
      pure (dir, flags, defs)

-- | Stitch the output text together from pretty-printed pieces: pragma
-- union, user module header, merged imports, then declarations of every
-- local module (dependency order) and finally the user's own.
assemble :: ProjectDefaults -> [ProjectDefaults] -> ParsedFile -> [LocalModule] -> String
assemble userDefaults libDefaults userFile locals =
  intercalate "\n\n" (filter (not . null) chunks) <> "\n"
  where
    chunks =
      [ intercalate "\n" pragmas
      , fromMaybe "" (renderModuleHeader (pfModule userFile))
      , intercalate "\n" imports
      ]
        <> map localChunk locals
        <> [intercalate "\n\n" (map renderDecl (declsOf userFile))]

    pragmas =
      nubOrd . concat $
        [ pdPragmas userDefaults
        , pfPragmas userFile
        , concatMap pdPragmas libDefaults
        , concatMap (pfPragmas . lmParsed) locals
        ]

    localNames = Set.fromList (map lmName locals)
    keptImports pf =
      [ imp
      | imp <- hsmodImports (unLoc (pfModule pf))
      , unLoc (ideclName (unLoc imp)) `Set.notMember` localNames
      ]
    imports =
      nubOrd . map renderImport . concat $
        keptImports userFile : map (keptImports . lmParsed) locals

    declsOf pf = hsmodDecls (unLoc (pfModule pf))

    localChunk lm =
      intercalate "\n\n" $
        ("-- ### " <> moduleNameString (lmName lm))
          : map renderDecl (declsOf (lmParsed lm))

-- | Re-parse our own output before emitting it: the pretty-printer is not
-- guaranteed to produce re-parseable code in every corner case, and a bundle
-- that does not even parse must never reach stdout silently.
selfCheck :: String -> IO (Either BundleError String)
selfCheck out = do
  reparsed <- parseHaskellFile baseDynFlags "<bundled output>" out
  pure $ case reparsed of
    Left err -> Left (SelfCheckError (renderBundleError err) out)
    Right _ -> Right out
