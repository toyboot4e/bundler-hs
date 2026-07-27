module Main (main) where

import Torture

main :: IO ()
main = do
  print (readInt "41")
  print (sumStrict [1, 2, 3])
  print (True --> False)
  print (classify (-1))
  print (lazySnd (1, 2))
  case [5 :: Int] of
    Single x -> print (x, Single x)
    _ -> pure ()
