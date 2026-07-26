module Main (main) where

import Decoy
import qualified Pretty as P

-- An instance of the local class, in the user's file: the method binder
-- must be renamed to match the renamed class method.
data Box = Box Int

instance P.Pretty Box where
  pretty (Box n) = "Box " <> P.pretty n
  indent _ = 2

-- An instance of an EXTERNAL class (Show): its method binder must stay
-- `show` even though the open import of Decoy puts a same-named local
-- export in unqualified scope.
newtype Tag = Tag Int

instance Show Tag where
  show (Tag n) = "#" <> display n

main :: IO ()
main = do
  putStrLn (P.pretty (Box 1))
  print (P.indent (Box 1))
  print (Tag 7)
