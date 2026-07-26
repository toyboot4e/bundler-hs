module Counter (Tally (..), count) where

import qualified Data.Map.Strict as M

data Tally = Tally
  { total :: Int
  , distinct :: Int
  }
  deriving (Show)

count :: String -> Tally
count s =
  let m = M.fromListWith (+) [(c, 1 :: Int) | c <- s]
   in Tally {total = length s, distinct = M.size m}
