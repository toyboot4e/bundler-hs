module Main (main) where

import Extra hiding (unused)
import Util (Pair (..), clamp, inc)

main :: IO ()
main = do
  print (inc 41)
  print (clamp 0 10 99)
  print (twice 21)
  let p = MkPair {left = 1, right = 2}
  print (left p + right p)
