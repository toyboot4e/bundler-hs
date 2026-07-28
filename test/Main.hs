module Main (main) where

import Bundler (bundle)
import Bundler.Config
  ( Config (..),
    EmbedPosition (..),
    FormatMode (..),
    parseConfigFromArgs,
  )
import Bundler.Error (BundleError (..), renderBundleError)
import Control.Monad (filterM, when)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List (isPrefixOf, sort)
import Data.Maybe (isJust)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Environment (lookupEnv, setEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

fixturesRoot :: FilePath
fixturesRoot = "test" </> "fixtures"

main :: IO ()
main = do
  compileGate <- isJust <$> lookupEnv "HSB_TEST_COMPILE"
  entries <- sort <$> listDirectory fixturesRoot
  dirs <- filterM (doesDirectoryExist . (fixturesRoot </>)) entries
  tests <- traverse (fixtureTest compileGate) dirs
  defaultMain
    ( testGroup
        "all"
        [ testGroup "golden" tests,
          testGroup "unit" [formatFailureSalvage]
        ]
    )

-- | A failing formatter must not lose the bundle: the pre-format output
-- is saved to a temp file whose content matches the --no-format output
-- exactly.
formatFailureSalvage :: TestTree
formatFailureSalvage = testCase "format failure saves the unformatted bundle" $ do
  systemTmp <- getTemporaryDirectory
  let sandbox = systemTmp </> "bundler-hs-salvage-test"
      fixture = fixturesRoot </> "format-fail"
      cfg format =
        Config
          { cfgInput = fixture </> "Main.hs",
            cfgSrcDirs = [],
            cfgRenameCmd = Nothing,
            cfgFormat = format,
            cfgEmbedPosition = EmbedAfter
          }
  createDirectoryIfMissing True sandbox
  mapM_ (removeFile . (sandbox </>)) =<< listDirectory sandbox
  -- Steer the salvage file into a sandbox we can inspect.
  setEnv "TMPDIR" sandbox
  raw <- bundle (cfg FormatNone)
  result <- bundle (cfg (FormatCmd "false"))
  setEnv "TMPDIR" systemTmp
  expected <- either (assertFailure . renderBundleError) pure raw
  case result of
    Right _ -> assertFailure "expected the failing formatter to abort bundling"
    Left (FormatCmdError {}) -> pure ()
    Left err -> assertFailure ("unexpected error: " <> renderBundleError err)
  saved <- filter (isPrefixOf "bundler-hs") <$> listDirectory sandbox
  case saved of
    [file] -> do
      contents <- readFile (sandbox </> file)
      assertEqual "salvaged bundle matches the unformatted output" expected contents
    _ ->
      assertBool ("expected exactly one salvaged bundle, found: " <> show saved) False
  removeDirectoryRecursive sandbox

-- | One golden test per fixture directory. The @args@ file holds
-- whitespace-separated CLI arguments; @{DIR}@ tokens and relative
-- input/src paths are rebased onto the fixture directory. A fixture with
-- @expected.err.golden@ asserts the rendered bundling error; otherwise
-- @expected.golden@ asserts stdout (and, with @HSB_TEST_COMPILE=1@, that
-- the bundle compiles under @ghc -fno-code@).
fixtureTest :: Bool -> FilePath -> IO TestTree
fixtureTest compileGate name = do
  let dir = fixturesRoot </> name
  errCase <- doesFileExist (dir </> "expected.err.golden")
  let golden = dir </> if errCase then "expected.err.golden" else "expected.golden"
  pure . goldenVsString name golden $ do
    args <- map (substDir dir) . words <$> readFile (dir </> "args")
    cfg <- either fail pure (parseConfigFromArgs args)
    let rebased =
          cfg
            { cfgInput = dir </> cfgInput cfg,
              cfgSrcDirs = map (dir </>) (cfgSrcDirs cfg)
            }
    result <- bundle rebased
    case (errCase, result) of
      (False, Right out) -> do
        when compileGate (assertCompiles name out)
        pure (LBS8.pack out)
      (False, Left err) -> fail ("unexpected bundling error:\n" <> renderBundleError err)
      (True, Left err) -> pure (LBS8.pack (renderBundleError err))
      (True, Right _) -> fail "expected a bundling error, but bundling succeeded"

substDir :: FilePath -> String -> String
substDir dir s = case s of
  [] -> []
  _
    | "{DIR}" `isPrefixOf` s -> dir <> substDir dir (drop 5 s)
    | (c : cs) <- s -> c : substDir dir cs

assertCompiles :: String -> String -> IO ()
assertCompiles name out = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp (name <> ".hs")
  hPutStr h out
  hClose h
  (code, _, ghcErr) <- readProcessWithExitCode "ghc" ["-fno-code", path] ""
  removeFile path
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> fail ("bundle does not compile:\n" <> ghcErr)
