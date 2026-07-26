module CountB (ranked) where

import Data.Char
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Q

-- Same external module as CountA but under a different alias; plus an
-- explicit-list import (dropped, uses qualified) and an open import
-- (kept verbatim).
ranked :: [Int] -> [(Int, String)]
ranked xs =
  [ (k, map toUpper v)
  | (k, v) <- Q.toList (Q.fromList (zip (sortBy (comparing negate) xs) ["a", "b", "c"]))
  ]
