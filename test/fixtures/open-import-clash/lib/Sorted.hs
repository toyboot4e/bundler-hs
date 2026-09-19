-- An open import: which names it brings in is unknowable, so the bundle
-- carries it as written, and it then governs the whole merged module.
module Sorted (merge) where

import Data.List

merge :: [Int] -> [Int]
merge xs = sort xs
