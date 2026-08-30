module Main (main) where

import Bundler (bundle)
import Bundler.Config (configParserInfo, parserPrefs)
import Bundler.Error (renderBundleError)
import Bundler.Utf8 (hSetUtf8)
import Options.Applicative
  ( ParserResult (..),
    execParserPure,
    handleParseResult,
    renderFailure,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (hPutStrLn, stderr, stdout)

main :: IO ()
main = do
  -- The bundle is UTF-8 source and so are the diagnostics quoting it.
  hSetUtf8 stdout
  hSetUtf8 stderr
  args <- getArgs
  let result = execParserPure parserPrefs configParserInfo args
  cfg <- case result of
    -- The brief usage error never mentions --help; point at it (the
    -- protocol notes in particular are only shown there).
    Failure failure
      | (msg, code@(ExitFailure _)) <- renderFailure failure "bundler-hs" -> do
          hPutStrLn stderr msg
          hPutStrLn stderr "\nRun 'bundler-hs --help' for more details."
          exitWith code
    _ -> handleParseResult result
  bundleResult <- bundle cfg
  case bundleResult of
    Left err -> do
      hPutStrLn stderr (renderBundleError err)
      exitFailure
    Right out -> putStr out
