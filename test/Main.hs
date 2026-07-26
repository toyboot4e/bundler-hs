module Main (main) where

import Bundler (bundle)
import Bundler.Config (Config (..), parseConfigFromArgs)
import Bundler.Error (renderBundleError)
import Control.Monad (filterM)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

fixturesRoot :: FilePath
fixturesRoot = "test" </> "fixtures"

main :: IO ()
main = do
  entries <- sort <$> listDirectory fixturesRoot
  dirs <- filterM (doesDirectoryExist . (fixturesRoot </>)) entries
  tests <- traverse fixtureTest dirs
  defaultMain (testGroup "golden" tests)

-- | One golden test per fixture directory. The @args@ file holds
-- whitespace-separated CLI arguments with paths relative to the fixture
-- directory. A fixture with @expected.err.golden@ asserts the rendered
-- bundling error; otherwise @expected.golden@ asserts stdout.
fixtureTest :: FilePath -> IO TestTree
fixtureTest name = do
  let dir = fixturesRoot </> name
  errCase <- doesFileExist (dir </> "expected.err.golden")
  let golden = dir </> if errCase then "expected.err.golden" else "expected.golden"
  pure . goldenVsString name golden $ do
    args <- words <$> readFile (dir </> "args")
    cfg <- either fail pure (parseConfigFromArgs args)
    let rebased =
          cfg
            { cfgInput = dir </> cfgInput cfg
            , cfgSrcDirs = map (dir </>) (cfgSrcDirs cfg)
            }
    result <- bundle rebased
    case (errCase, result) of
      (False, Right out) -> pure (LBS8.pack out)
      (False, Left err) -> fail ("unexpected bundling error:\n" <> renderBundleError err)
      (True, Left err) -> pure (LBS8.pack (renderBundleError err))
      (True, Right _) -> fail "expected a bundling error, but bundling succeeded"
