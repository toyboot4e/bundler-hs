module Main (main) where

-- Legal Haskell (GHC unions the scope of a shared alias), but the bundler
-- cannot attribute X.count / X.total reliably and must refuse.
import qualified CounterA as X
import qualified CounterB as X

main :: IO ()
main = print (X.count [1, 2], X.total [1, 2])
