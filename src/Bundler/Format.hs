module Bundler.Format
  ( formatBuiltin,
  )
where

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import HIndent (Config (..), defaultConfig, reformat)
import HIndent qualified

-- | Format with the hindent library (shared ghc-lib-parser, no external
-- binary). hindent re-decides every line break, which is exactly what the
-- GHC pretty-printer's output needs.
formatBuiltin :: String -> Either String String
formatBuiltin src =
  case reformat defaultConfig {configMaxColumns = 100} HIndent.defaultExtensions Nothing utf8 of
    Left err -> Left (show err)
    Right formatted -> Right (T.unpack (T.decodeUtf8Lenient formatted))
  where
    utf8 = T.encodeUtf8 (T.pack src)
