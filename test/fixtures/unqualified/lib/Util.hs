module Util (Pair (..), clamp, inc) where

data Pair = MkPair
  { left :: Int
  , right :: Int
  }

inc :: Int -> Int
inc = (+ 1)

clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

-- Not exported; must still be renamed and stay callable from clamp's
-- module, but is invisible to importers.
private :: Int
private = 0
