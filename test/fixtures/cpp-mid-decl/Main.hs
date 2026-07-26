{-# LANGUAGE CPP #-}

module Main (main) where

main :: IO ()
main = do
  n <- readLn :: IO Int
#ifdef DEBUG
  print ("debug", n)
#endif
  print (n * 2)
