module Bundler.Discovery
  ( LocalModule (..)
  , discoverLocalModules
  , importedModules
  ) where

import Bundler.Error
import Bundler.Parse
import Control.Monad (filterM, foldM)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Driver.Session (DynFlags)
import GHC.Hs (hsmodImports, ideclName)
import GHC.Types.SrcLoc (unLoc)
import Language.Haskell.Syntax.Module.Name (ModuleName, moduleNameSlashes, moduleNameString)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
import System.IO (readFile')

-- | One local library module pulled into the bundle.
data LocalModule = LocalModule
  { lmName :: ModuleName
  , lmParsed :: ParsedFile
  , lmDeps :: [ModuleName]
  -- ^ Its imports that are themselves local modules.
  }

-- | All modules a parsed file imports, local or not.
importedModules :: ParsedFile -> [ModuleName]
importedModules pf =
  map (unLoc . ideclName . unLoc) (hsmodImports (unLoc (pfModule pf)))

-- | An import is local iff its source file exists under exactly one
-- @--src@ dir; under several it is ambiguous (error), under none it is
-- external and stays an import.
lookupLocal :: [(FilePath, DynFlags)] -> ModuleName -> IO (Either BundleError (Maybe (FilePath, DynFlags)))
lookupLocal srcDirs m = do
  hits <- filterM (doesFileExist . fst) [(pathIn d, flags) | (d, flags) <- srcDirs]
  pure $ case hits of
    [] -> Right Nothing
    [hit] -> Right (Just hit)
    several -> Left (DuplicateModule (moduleNameString m) (map fst several))
  where
    pathIn d = d </> moduleNameSlashes m <.> "hs"

-- | Breadth-first expansion of local imports starting from the user's file,
-- returning modules in dependency order (dependencies before dependents).
-- Each local file is parsed with the 'DynFlags' of the @--src@ dir it was
-- found under (carrying that project's cabal defaults).
discoverLocalModules
  :: [(FilePath, DynFlags)]
  -> ParsedFile
  -> IO (Either BundleError [LocalModule])
discoverLocalModules srcDirs userFile = do
  result <- go Map.empty (importedModules userFile)
  pure (result >>= topoSort)
  where
    go :: Map ModuleName LocalModule -> [ModuleName] -> IO (Either BundleError (Map ModuleName LocalModule))
    go seen [] = pure (Right seen)
    go seen (m : rest)
      | m `Map.member` seen = go seen rest
      | otherwise = do
          hit <- lookupLocal srcDirs m
          case hit of
            Left err -> pure (Left err)
            Right Nothing -> go seen rest
            Right (Just (path, flags)) -> do
              src <- readFile' path
              parsed <- parseHaskellFile flags path src
              case parsed of
                Left err -> pure (Left err)
                Right pf -> do
                  localDeps <- classify (importedModules pf)
                  case localDeps of
                    Left err -> pure (Left err)
                    Right deps -> do
                      let lm = LocalModule {lmName = m, lmParsed = pf, lmDeps = deps}
                      go (Map.insert m lm seen) (deps <> rest)

    classify :: [ModuleName] -> IO (Either BundleError [ModuleName])
    classify = foldM step (Right [])
      where
        step (Left err) _ = pure (Left err)
        step (Right acc) m = do
          hit <- lookupLocal srcDirs m
          pure (fmap (\h -> acc <> maybe [] (const [m]) h) hit)

topoSort :: Map ModuleName LocalModule -> Either BundleError [LocalModule]
topoSort seen = concat <$> traverse fromSCC sccs
  where
    sccs =
      stronglyConnComp
        [ (lm, lmName lm, lmDeps lm)
        | lm <- Map.elems seen
        ]
    fromSCC (AcyclicSCC lm) = Right [lm]
    fromSCC (CyclicSCC lms) = Left (ImportCycle (map (moduleNameString . lmName) lms))
