module Main (main) where

import Caller (call)
import Helper (twice)
import Util
import qualified Util as U

-- Three shapes at once. Util is imported both ways in one file and the
-- qualified half settles it. Helper is only ever imported plainly here,
-- but Caller names it qualified, which settles it too. Caller itself is
-- only ever imported plainly, so `call` keeps its name.
main :: IO ()
main = print (inc 1 + U.dec 2 + twice 3 + call 4)
