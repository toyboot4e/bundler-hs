module Algo.C (sumUp) where

-- LambdaCase comes from the library project's default-extensions, not an
-- in-file pragma: this file only parses if cabal extraction works.
sumUp :: [Int] -> Int
sumUp = sum . map (\case 0 -> 1; n -> n)
