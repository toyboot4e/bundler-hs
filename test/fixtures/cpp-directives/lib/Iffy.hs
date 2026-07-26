{-# LANGUAGE CPP #-}

module Iffy (answer) where

answer :: Int
#ifdef WIN32
answer = 1
#else
answer = 2
#endif
