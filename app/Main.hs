module Main (main) where

import Bundler (bundle)
import Bundler.Config (configParserInfo)
import Bundler.Error (renderBundleError)
import Options.Applicative (execParser)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  cfg <- execParser configParserInfo
  result <- bundle cfg
  case result of
    Left err -> do
      hPutStrLn stderr (renderBundleError err)
      exitFailure
    Right out -> putStr out
