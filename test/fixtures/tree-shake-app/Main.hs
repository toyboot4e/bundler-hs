module Main (main) where

import Algo (solve)

-- A helper left over from an earlier attempt. Nothing calls it any more,
-- so it leaves together with the comment written above it.
leftover :: Int -> Int
leftover n = n * 3

data Unused = Unused Int

-- This one is still called, so this comment stays.
answer :: Int
answer = 3

main :: IO ()
main = print (solve [1, 2, answer])
