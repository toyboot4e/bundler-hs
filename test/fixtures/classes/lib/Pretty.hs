module Pretty (Pretty (..)) where

class Pretty a where
  pretty :: a -> String
  indent :: a -> Int
  indent _ = 0

instance Pretty Int where
  pretty = show
