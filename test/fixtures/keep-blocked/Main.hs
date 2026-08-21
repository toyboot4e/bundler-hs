module Main (main) where

import Blocked (kept)
import Data.List (sort)
import Rival (other)

collide :: Int
collide = 7

main :: IO ()
main = do
  print (sort [2, 1 :: Int], collide)
  print (kept 1, other 2)
