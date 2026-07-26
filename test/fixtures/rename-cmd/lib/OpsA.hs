module OpsA (f, (<+>)) where

infixl 6 <+>

(<+>) :: Int -> Int -> Int
(<+>) = (+)

f :: Int -> Int
f x = x <+> 10
