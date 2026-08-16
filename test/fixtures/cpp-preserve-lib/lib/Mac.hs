{-# LANGUAGE CPP #-}

-- No macros come from this library's project, so its conditionals are left
-- for whatever compiler builds the bundle. Both branches are renamed.
module Mac (debug) where

#ifdef DEBUG
debug :: Bool
debug = True
#else
debug :: Bool
debug = False
#endif
