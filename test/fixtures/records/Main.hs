{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Rec (Point (..), norm)

shift :: Point -> Point
shift p = p {px = px p + 1}

viaPuns :: Point -> Int
viaPuns Point {px} = px

viaWildcards :: Point -> Int
viaWildcards Point {..} = px + py

build :: Int -> Point
build n =
  let px = n
      py = n * 2
   in Point {..}

main :: IO ()
main = do
  let p = Point {px = 3, py = 4}
  print (norm p)
  print (norm (shift p))
  print (viaPuns p)
  print (viaWildcards p)
  print (norm (build 5))
