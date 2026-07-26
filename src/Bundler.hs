module Bundler
  ( bundle
  ) where

import Bundler.Config
import Bundler.Error
import Bundler.Parse
import Bundler.Render
import System.IO (readFile')

-- | Run the whole pipeline, producing the bundled source for stdout.
--
-- Current stage (M0): parse the input file and pretty-print it back
-- (identity bundle). Discovery, renaming, and import merging are layered on
-- top of this in later milestones.
bundle :: Config -> IO (Either BundleError String)
bundle cfg = do
  src <- readFile' (cfgInput cfg)
  parsed <- parseHaskellFile baseDynFlags (cfgInput cfg) src
  case parsed of
    Left err -> pure (Left err)
    Right pf -> selfCheck (assemble pf)

-- | Assemble the output text from its pieces.
assemble :: ParsedFile -> String
assemble pf =
  concatMap (<> "\n") (pfPragmas pf)
    <> renderModule (pfModule pf)
    <> "\n"

-- | Re-parse our own output before emitting it: the pretty-printer is not
-- guaranteed to produce re-parseable code in every corner case, and a bundle
-- that does not even parse must never reach stdout silently.
selfCheck :: String -> IO (Either BundleError String)
selfCheck out = do
  reparsed <- parseHaskellFile baseDynFlags "<bundled output>" out
  pure $ case reparsed of
    Left err -> Left (SelfCheckError (renderBundleError err) out)
    Right _ -> Right out
