{-# LANGUAGE CPP #-}
-- A LANGUAGE pragma behind a conditional: it must still reach the bundle's
-- pragma block, or the code it enables will not compile there.
#if __GLASGOW_HASKELL__ >= 900
{-# LANGUAGE TupleSections #-}
#endif

module Guarded (pairUp) where

pairUp :: Int -> (Int, Int)
pairUp = (, 1)
