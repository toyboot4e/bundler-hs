module Main (main) where

import qualified Plain

main :: IO ()
main = print (Plain.answer + 1)
