module Deque (push, pop) where

push :: Int -> [Int] -> [Int]
push = (:)

pop :: [Int] -> [Int]
pop = drop 1
