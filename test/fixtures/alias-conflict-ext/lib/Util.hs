module Util (sizes) where

-- Two external modules under one alias: rewriting M.size to a canonical
-- qualifier would silently pick one of them, so this must be an error.
import qualified Data.Map.Strict as M
import qualified Data.Set as M

sizes :: Int
sizes = M.size (M.fromList [(1 :: Int, ())])
