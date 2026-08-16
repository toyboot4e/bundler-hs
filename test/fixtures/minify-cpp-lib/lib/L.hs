{-# LANGUAGE CPP #-}

-- A preserved conditional sits between declarations. Minifying the library
-- section must still put everything outside it on one line, which means the
-- conditional moves to the end.
module L (a, b, debug, c, d) where

a :: Int
a = 1

b :: Int
b = 2

#ifdef DEBUG
debug :: Bool
debug = True
#else
debug :: Bool
debug = False
#endif

c :: Int
c = 3

d :: Int
d = 4
