module Mac (debug) where

#ifdef DEBUG
debug :: Bool
debug = True
#else
debug :: Bool
debug = False
#endif
