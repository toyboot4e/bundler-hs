module OpB (f, (<+>)) where

(<+>) :: Int -> Int -> Int
(<+>) = (*)

f :: Int -> Int
f x = x <+> 2
