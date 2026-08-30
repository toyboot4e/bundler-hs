{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Rec (Point (..), build, dump) where

data Point = Point
  { px :: Int
  , py :: Int
  }

-- A pun and a wildcard in one record construction: the wildcard must fill
-- in py alone, because px is already named.
build :: Int -> Point
build n =
  let px = n
      py = n + 1
   in Point {px, ..}

-- The same in a record pattern.
dump :: Point -> Int
dump Point {px, ..} = px + py
