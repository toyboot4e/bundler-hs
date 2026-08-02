module Main (main) where

-- Externals sharing an alias in the user's own file survive verbatim, so
-- this must keep bundling (GHC unions the scope; the uses are unambiguous).
import qualified Data.Char as C
import qualified Data.List as C
import Util (double)

main :: IO ()
main = do
  print (C.toUpper 'a')
  print (C.sort [3, 1, 2 :: Int])
  print (double 21)
