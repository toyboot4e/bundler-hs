{-# LANGUAGE CPP #-}

-- The bundle is compiled inside the user's project, so this is resolved with
-- the macros that project's cpp-options put in force, not this library's.
module Mac (debug) where

#ifdef DEBUG
debug :: Bool
debug = True
#else
debug :: Bool
debug = False
#endif
