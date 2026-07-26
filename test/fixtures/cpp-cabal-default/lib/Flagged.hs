-- No in-file CPP pragma: the extension comes from the library project's
-- cabal default-extensions.
module Flagged (value) where

value :: Int
#ifdef SPECIAL
value = 1
#else
value = 2
#endif
