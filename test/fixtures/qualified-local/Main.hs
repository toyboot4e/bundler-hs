module Main (main) where

import qualified Alpha as A
import qualified Beta

main :: IO ()
main = do
  let h = A.push 1 A.emptyHeap
  print (A.f h)
  print (A.size h)
  print (Beta.f 41)
