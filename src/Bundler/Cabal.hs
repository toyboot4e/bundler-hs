module Bundler.Cabal
  ( ProjectDefaults (..)
  , findProjectDefaults
  ) where

import Bundler.Error
import Data.ByteString qualified as BS
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (maybeToList)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty (prettyShow)
import Distribution.Types.Benchmark (benchmarkBuildInfo)
import Distribution.Types.BuildInfo (BuildInfo, defaultExtensions, defaultLanguage)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.Executable (buildInfo)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library (libBuildInfo)
import Distribution.Types.TestSuite (testBuildInfo)
import System.Directory (canonicalizePath, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

-- | The language defaults a source tree inherits from its enclosing cabal
-- project. The defaults are synthesized into LANGUAGE pragma lines so that
-- one code path (GHC's own pragma parser) interprets cabal defaults, and the
-- same lines can be re-emitted into the bundle's pragma block.
data ProjectDefaults = ProjectDefaults
  { pdRoot :: FilePath
  -- ^ Directory holding the @.cabal@ file, or the starting directory when no
  -- project was found.
  , pdPragmas :: [String]
  -- ^ Synthesized @{-\# LANGUAGE ... \#-}@ lines: @default-language@ first
  -- (editions like GHC2021 are valid pragmas), then @default-extensions@,
  -- unioned over every stanza and conditional branch.
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
                then pure (Right (ProjectDefaults start []))
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
          { pdRoot = root
          , pdPragmas = map mkPragma (nubOrd (languages <> extensions))
          }
      where
        infos = allBuildInfos gpd
        languages = map prettyShow (concatMap (maybeToList . defaultLanguage) infos)
        extensions = map prettyShow (concatMap defaultExtensions infos)

mkPragma :: String -> String
mkPragma s = "{-# LANGUAGE " <> s <> " #-}"

-- | Every 'BuildInfo' in the package, taking every conditional branch: the
-- bundler cannot know which flags/OS the user builds with, so defaults are
-- unioned over all of them (additive, per spec).
allBuildInfos :: GenericPackageDescription -> [BuildInfo]
allBuildInfos gpd =
  concat
    [ map libBuildInfo (flattenAll (maybeToList (condLibrary gpd)))
    , map libBuildInfo (flattenAll (map snd (condSubLibraries gpd)))
    , map buildInfo (flattenAll (map snd (condExecutables gpd)))
    , map testBuildInfo (flattenAll (map snd (condTestSuites gpd)))
    , map benchmarkBuildInfo (flattenAll (map snd (condBenchmarks gpd)))
    ]
  where
    flattenAll = concatMap flattenCondTree

flattenCondTree :: CondTree v c a -> [a]
flattenCondTree (CondNode a _ branches) = a : concatMap flattenBranch branches
  where
    flattenBranch (CondBranch _ true mfalse) =
      flattenCondTree true <> maybe [] flattenCondTree mfalse
