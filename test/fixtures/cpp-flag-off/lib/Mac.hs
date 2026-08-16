{-# LANGUAGE CPP #-}

-- This library project supplies MARKER, a macro that disappears with the
-- package, so the module is resolved at bundle time rather than preserved.
module Mac (debug, marker) where

marker :: Int
marker = MARKER

#ifdef DEBUG
debug :: Bool
debug = True
#else
debug :: Bool
debug = False
#endif
