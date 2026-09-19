module Main (main) where

import Sorted (merge)
import Store (insert, size)

main :: IO ()
main = print (insert 1 [2], size [1, 2], merge [3, 1])
