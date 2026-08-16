{-# LANGUAGE CPP #-}

-- The guarded import is hoisted into the bundle's import block, so the
-- conditional around it would be left enclosing nothing.
module CondImp (v) where

#ifdef USE_SORT
import Data.List (sort)
#endif

v :: [Int]
#ifdef USE_SORT
v = sort [3, 1, 2]
#else
v = [3, 1, 2]
#endif
