module Main (main) where

import Shaken (Kept (..), used)

main :: IO ()
main = putStrLn (used (Kept 1))
