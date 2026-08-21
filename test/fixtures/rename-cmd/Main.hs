module Main (main) where

import qualified Counter as C
import qualified OpsA as A
import qualified OpsB as B
import Plain (plain)

main :: IO ()
main = do
  print (A.f 1)
  print (B.g 2)
  print (C.count "abracadabra")
  print (plain 3)
