{-# LANGUAGE CPP #-}

module Cond (pick, dead) where

#ifdef FAST
pick :: Int
pick = 1
#else
pick :: Int
pick = 2
#endif

dead :: Int
dead = 3
