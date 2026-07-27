module Shadow (f, g) where

f :: Int -> Int
f = (+ 100)

-- Internal shadowing inside the library module itself: the lambda-bound f
-- stays, the call to top-level f in the where clause is renamed.
g :: Int -> Int
g x = inner (\f -> f x)
  where
    inner k = k step
    step y = f y - 100
