{-# LANGUAGE CPP #-}

module Main (main) where

import qualified Alg

#ifdef LOCAL
#define DBG(x) (print (x))
#else
#define DBG(x) (pure ())
#endif

main :: IO ()
main = do
  n <- readLn :: IO Int
  DBG(Alg.inc n)
  print (Alg.inc n)
