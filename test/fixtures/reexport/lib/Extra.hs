-- module-self export plus a re-export of an imported module
module Extra (module Extra, module Core) where

import Core

shrink :: Int -> Int
shrink x = flatten (grow x) - 1
