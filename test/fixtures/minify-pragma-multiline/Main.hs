{-# LANGUAGE LambdaCase,
             TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

main :: IO ()
main =
  print (map (\case
                0 -> (1 :: Int)
                n -> n) [0, 2])
