-- top-of-file comment (part of the header block, kept above the pragmas)
module Main (main) where

import Alpha (emptyHeap, f, push)

-- a comment between imports
import Data.List (sort)

-- | Doc comment on the solver.
solve :: [Int] -> Int
solve xs = f (foldr push emptyHeap (sort xs)) -- trailing comment

{- a block comment
   between declarations -}
main :: IO ()
main = print (solve [3, 1, 2])

-- closing remark
