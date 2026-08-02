module Main (main) where

import Bulk
import Bulk qualified as B
import Core

main :: IO ()
main = do
  print (shrink 1)
  -- reaches Main through Bulk -> Extra -> Core
  print (grow 2)
  -- in scope from both Core and Bulk: same origin, not ambiguous
  print (flatten 3)
  -- qualified access through two levels of re-export
  print (B.grow 4)
