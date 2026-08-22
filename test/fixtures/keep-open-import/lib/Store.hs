module Store (insert, size, partition) where

-- `insert` is also what Data.List calls one of its exports, and Sorted
-- imports that openly, so this one has to move out of the way. Nothing in
-- the bundle writes `size`, so that one stays as it is.
insert :: Int -> [Int] -> [Int]
insert = (:)

size :: [Int] -> Int
size = length

-- Data.List exports a `partition` too, but nothing in the bundle writes
-- that one, so this name is kept and hidden from the open import instead.
partition :: [Int] -> ([Int], [Int])
partition xs = (xs, [])
