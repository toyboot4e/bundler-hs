module Main (main) where

import qualified OpA
import qualified OpB

main :: IO ()
main = print (OpA.f 1, OpB.f 2)
