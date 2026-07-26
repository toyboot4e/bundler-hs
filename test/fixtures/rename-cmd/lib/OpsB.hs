module OpsB (g, (<+>)) where

(<+>) :: Int -> Int -> Int
(<+>) = (*)

g :: Int -> Int
g x = x <+> 10
