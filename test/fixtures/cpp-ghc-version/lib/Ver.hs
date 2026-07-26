{-# LANGUAGE CPP #-}

module Ver (spec) where

-- Library code branching on compiler version: evaluated at bundle time
-- with __GLASGOW_HASKELL__ = 912.
spec :: String
#if __GLASGOW_HASKELL__ >= 900
spec = "modern"
#else
spec = "ancient"
#endif
