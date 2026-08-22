module Caller (call) where

-- The only qualified import of Helper in the bundle is this one, in a
-- library module: it settles Helper's names all the same.
import qualified Helper as H

call :: Int -> Int
call = H.twice
