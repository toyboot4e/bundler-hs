module Main (main) where

import Vec ((<+>), scale)
import qualified Vec as V

main :: IO ()
main = do
  print ((1, 2) <+> (3, 4))
  print ((1, 2) V.<+> V.origin)
  print (scale 2 (3, 4))
  print (foldr (<+>) V.origin [(1, 1), (2, 2)])
