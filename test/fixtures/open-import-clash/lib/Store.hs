module Store (partition) where

-- Data.List exports a `partition` too, but nothing in the bundle writes
-- that one, so this name is kept as it is - and Sorted's open import puts
-- the other one in scope beside it.
partition :: [Int] -> ([Int], [Int])
partition xs = (xs, [])
