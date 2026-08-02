module Main (main) where

import Reader (readPair)

main :: IO ()
main = do
  -- Prelude's read must survive the bundling.
  print (read "42" :: Int)
  print (readPair "abc")
