module Main (main) where

import qualified Exotic as E

main :: IO ()
main = do
  print (E.eval (E.AddE (E.IntE 1) (E.IntE 2)))
  print (E.eval (E.BoolE True))
  print (E.insert (3 :: Int) E.empty :: [Int])
