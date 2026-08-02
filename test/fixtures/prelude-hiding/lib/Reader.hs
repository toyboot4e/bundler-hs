module Reader (read, readPair) where

import Prelude hiding (read)

-- The module's own read shadows Prelude's, hence the hiding; after
-- renaming the hiding must not leak into the bundle.
read :: String -> Int
read s = length s

readPair :: String -> (Int, Int)
readPair s = (read s, read (reverse s))
