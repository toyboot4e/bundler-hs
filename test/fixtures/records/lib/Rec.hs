{-# LANGUAGE RecordWildCards #-}

module Rec (Point (..), norm) where

data Point = Point
  { px :: Int
  , py :: Int
  }

-- Wildcards inside the defining module itself.
norm :: Point -> Int
norm Point {..} = px * px + py * py
