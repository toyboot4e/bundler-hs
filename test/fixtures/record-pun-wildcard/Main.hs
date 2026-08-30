module Main (main) where

import qualified Rec as R

main :: IO ()
main = print (R.dump (R.build 1))
