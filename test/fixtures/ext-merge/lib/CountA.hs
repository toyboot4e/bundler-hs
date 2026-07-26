module CountA (countUp) where

import qualified Data.Map.Strict as M

countUp :: [Int] -> [(Int, Int)]
countUp xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])
