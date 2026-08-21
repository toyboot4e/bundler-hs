module Rival (shared, other) where

shared :: Int -> Int
shared = negate

other :: Int -> Int
other = shared
