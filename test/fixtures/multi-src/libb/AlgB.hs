module AlgB (thrice) where

import AlgA (twice)

thrice :: Int -> Int
thrice n = twice n + n
