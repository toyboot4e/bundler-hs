{-# LANGUAGE CPP #-}

module Main (main) where

import qualified Alg

#ifdef LOCAL
debug :: Show a => a -> IO ()
debug = print
#else
debug :: Show a => a -> IO ()
debug _ = pure ()
#endif

main :: IO ()
main = do
  debug (Alg.inc 1)
  print (Alg.inc 41)
