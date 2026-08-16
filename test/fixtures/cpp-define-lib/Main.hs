module Main (main) where

import qualified Macro

main :: IO ()
main =
  print
    (Macro.limit, Macro.twice 4, Macro.total, Macro.flagged, Macro.state, Macro.label)
