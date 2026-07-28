{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Alg

-- User code stays hindent-formatted while the library and imports are
-- minified.
solve :: [Int] -> [Int]
solve = sortBy (comparing negate) . map (\case 0 -> Alg.inc 0; n -> n)

main :: IO ()
main = print (solve [3, 0, 2, 1])
