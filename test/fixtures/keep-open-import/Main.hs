module Main (main) where

import Sorted (merge)
import Store (insert, partition, size)

main :: IO ()
main = print (insert 1 [2], size [1, 2], partition [4], merge [3, 1])
