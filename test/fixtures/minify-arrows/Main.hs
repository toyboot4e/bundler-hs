module Main (main) where

import qualified Arrowy

main :: IO ()
main = print (Arrowy.pipeline 20)
