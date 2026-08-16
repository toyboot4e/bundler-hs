{-# LANGUAGE CPP #-}

-- In library modules the directives are evaluated at bundle time, so the
-- #define lines themselves never reach the bundle: only what they expand to.
module Macro (limit, twice, total, flagged, state, label) where

#define LIMIT 1000000007
#define DOUBLE(x) ((x) + (x))

#ifndef SEEN
#define SEEN
#define SUM3(a, b, c) \
  ((a) + (b) + (c))
#endif

#define ENABLED
#define TEMPORARY
#undef TEMPORARY

limit :: Int
limit = LIMIT

twice :: Int -> Int
twice n = DOUBLE(n)

total :: Int
total = SUM3(1, 2, 3)

#ifdef ENABLED
flagged :: Bool
flagged = True
#else
flagged :: Bool
flagged = False
#endif

#ifdef TEMPORARY
state :: Int
state = 1
#else
state :: Int
state = 0
#endif

-- A macro name inside a string or a character literal is left alone.
label :: (String, Char)
label = ("LIMIT stays", 'X')
