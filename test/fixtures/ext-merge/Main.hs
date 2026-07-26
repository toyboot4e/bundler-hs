module Main (main) where

import qualified CountA
import qualified CountB
import qualified Data.Map.Strict as MyMap

main :: IO ()
main = do
  print (CountA.countUp [1, 2, 2, 3])
  print (CountB.ranked [3, 1, 2])
  print (MyMap.singleton (1 :: Int) "user alias survives")
