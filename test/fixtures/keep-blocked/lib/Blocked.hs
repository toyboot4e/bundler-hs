-- Only `kept` gets to keep its spelling. Every other name here is claimed
-- by something the bundler cannot move out of the way: Prelude, the user's
-- own import list, the user's own top level, and a rival local module.
module Blocked (kept, lookup, sort, collide, shared) where

import Prelude hiding (lookup)

lookup :: Int -> Int
lookup = id

sort :: Int -> Int
sort = id

collide :: Int -> Int
collide = id

shared :: Int -> Int
shared = id

kept :: Int -> Int
kept n = lookup n + sort n + collide n + shared n
