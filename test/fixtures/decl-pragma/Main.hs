-- A file with no module header: a declaration pragma at column 1 must stay
-- with its binding instead of being lifted into the bundle's header.
import qualified Alg

{-# INLINE solve #-}
solve :: Int -> Int
solve = Alg.inc

main :: IO ()
main = print (solve 1)
