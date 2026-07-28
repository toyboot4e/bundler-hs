module Main (main) where

import qualified AlgA as A
import qualified AlgB as B

main :: IO ()
main = print (A.twice 3 + B.thrice 4)
