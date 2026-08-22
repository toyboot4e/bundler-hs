module Sorted (merge) where

-- An open import: which names it brings in is unknowable, but `insert`
-- below is written and no local module defines it.
import Data.List

merge :: [Int] -> [Int]
merge xs = insert 0 (sort xs)
