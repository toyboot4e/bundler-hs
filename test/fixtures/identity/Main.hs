{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M

solve :: [Int] -> Int
solve = sum . map (\case 0 -> 1; n -> n)

frequencies :: [Int] -> M.Map Int Int
frequencies xs = M.fromListWith (+) [(x, 1) | x <- xs]

main :: IO ()
main = do
  let xs = [3, 1, 0, 2] :: [Int]
  print (solve xs)
  print (sortBy (comparing negate) xs)
  print (frequencies xs)
