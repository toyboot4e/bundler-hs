module Main (main) where

import qualified Algo.A as A
import qualified Algo.B as B

main :: IO ()
main = do
  print (A.solveA [1, 2, 3])
  print (B.solveB [1, 2, 3])
