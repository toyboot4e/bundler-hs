module Store (insert, size) where

-- `insert` is also what Data.List calls one of its exports, and Sorted
-- imports that openly, so this one has to move out of the way. Nothing in
-- the bundle writes `size`, so that one stays as it is - Data.List does
-- not export it, and the open import Sorted wrote is carried unchanged.
insert :: Int -> [Int] -> [Int]
insert = (:)

size :: [Int] -> Int
size = length
