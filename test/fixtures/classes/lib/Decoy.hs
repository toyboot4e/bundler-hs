module Decoy (display, show) where

display :: Int -> String
display n = "<" <> Prelude.show n <> ">"

-- A decoy named like Prelude's class method, exported on purpose. The
-- top-level definition legitimately shadows the implicit Prelude import.
show :: Int -> String
show = display
