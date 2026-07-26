module Main (main) where

import Shadow (f, g)

-- Every f below except the imported one is a local binder and must NOT be
-- renamed; the g calls resolve to the import and MUST be.
lam :: (Int -> Int) -> Int
lam = \f -> f (g 1)

letBound :: Int
letBound = let f = (+ 2) in f (g 1)

whereBound :: Int
whereBound = f' + g 3
  where
    f' = f 10
    localHelper f = f 4

caseAlt :: Maybe (Int -> Int) -> Int
caseAlt m = case m of
  Just f -> f (g 5)
  Nothing -> f 6

doBind :: IO Int
doBind = do
  let g' = g
  f <- pure g'
  pure (f 7)

comprehension :: [Int]
comprehension = [f | f <- map g [1, 2, 3], f > 0]

patternGuard :: Maybe Int -> Int
patternGuard m
  | Just f <- m, f > 0 = f + g 8
  | otherwise = f 9

main :: IO ()
main = do
  n <- doBind
  print
    [ lam succ
    , letBound
    , whereBound
    , caseAlt Nothing
    , n
    , sum comprehension
    , patternGuard (Just 1)
    ]
