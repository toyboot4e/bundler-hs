module Bitsy (ones) where

-- An import list the bundler cannot expand, because the children of
-- Bits(..) are unknowable without the package. The import is kept as
-- written, and a list that names what it brings in cannot also hide, so
-- nothing may be bolted onto it.
import Data.Bits (Bits (..))

ones :: Int -> Int
ones = popCount
