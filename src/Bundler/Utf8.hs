-- | UTF-8 file access.
--
-- Haskell source is UTF-8 by definition and a cabal file is too, so neither
-- may be read through the locale encoding: under a non-UTF-8 locale that
-- either mangles every non-ASCII character or fails outright, with an
-- 'IOError' the bundler has no way to report as a 'Bundler.Error.BundleError'.
module Bundler.Utf8
  ( readUtf8File,
    hSetUtf8,
  )
where

import System.IO (Handle, IOMode (..), hGetContents', hSetEncoding, openFile, utf8)

-- | Read a whole file as UTF-8, whatever the locale says.
readUtf8File :: FilePath -> IO String
readUtf8File path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  -- Strict, and closes the handle.
  hGetContents' h

-- | Pin one handle to UTF-8, for the same reason.
hSetUtf8 :: Handle -> IO ()
hSetUtf8 h = hSetEncoding h utf8
