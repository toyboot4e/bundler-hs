module Shaken (Pretty (..), Kept (..), Dropped (..), used, unused, (+++)) where

class Pretty a where
  pretty :: a -> String

data Kept = Kept Int

instance Pretty Kept where
  pretty (Kept n) = "kept " <> show n

-- Nothing outside this module reaches Dropped, so the type and the only
-- instance that mentions it go together.
data Dropped = Dropped Int

instance Pretty Dropped where
  pretty (Dropped n) = "dropped " <> show n

infixl 6 +++

(+++) :: Int -> Int -> Int
a +++ b = a + b

{-# INLINE used #-}
used :: Kept -> String
used k = pretty k <> show (1 +++ 2)

-- Reachable only from `unused`, which nothing reaches either.
helper :: Int -> Int
helper = (* 2)

unused :: Int -> Int
unused n = helper n + 1
