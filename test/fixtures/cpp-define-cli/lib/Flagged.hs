module Flagged (value) where

#ifdef DEBUG
value :: Int
value = 0
#elif defined(VERBOSE)
value :: Int
value = LEVEL
#else
value :: Int
value = 9
#endif
