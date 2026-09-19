module Main (main) where

import Sorted (merge)
import Store (partition)

main :: IO ()
main = print (partition [4], merge [3, 1])
