{-# LANGUAGE Arrows #-}

module Arrowy (pipeline) where

import Control.Arrow

pipeline :: Int -> Int
pipeline = runId $ proc x -> do
  let y = x + 1
  z <- arr (* 2) -< y
  case even z of
    True -> returnA -< z
    False -> returnA -< z + 1
  where
    runId f = f

runKleisliLike :: (Int -> Int) -> Int -> Int
runKleisliLike f = f
