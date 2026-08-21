module Main (main) where

import qualified Data.Stack

-- The user's own names are untouchable, so they knock out the short rungs
-- of the suffix ladder one by one: S, then Stack, leaving DataStack.
pushS :: Int
pushS = 0

pushStack :: Int
pushStack = 1

main :: IO ()
main = do
  print (pushS, pushStack)
  print (Data.Stack.push 1 [])
