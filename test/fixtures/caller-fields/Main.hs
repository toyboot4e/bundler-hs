{-# LANGUAGE DisambiguateRecordFields #-}

module Main (main) where

import qualified Mod as Q

-- Qualified-ONLY import: the field labels below are legal unqualified
-- solely because DisambiguateRecordFields resolves them via the
-- constructor. The bundler must rename them via the constructor too.
box :: Q.Box
box = Q.MkBox {content = 41, label = "x"}

grab :: Q.Box -> Int
grab Q.MkBox {content = c} = c

main :: IO ()
main = do
  print (grab box)
  print (Q.label box)
