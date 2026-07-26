{-# LANGUAGE DisambiguateRecordFields #-}

module Main (main) where

import Mod
import qualified Mod as Q

-- Caller-side references in every position: direct call, backtick infix,
-- partial application, type-level, pattern, guard, where, comprehension.
direct :: Int
direct = myFunc 21

viaBacktick :: Int
viaBacktick = 1 `myOther` 2

partial :: [Int]
partial = map myFunc [1, 2, 3]

qualifiedCall :: Int
qualifiedCall = Q.myFunc (Q.myOther 1 2)

typeLevel :: Box -> Int
typeLevel b = content b

inPattern :: Box -> String
inPattern MkBox {label = l} = l

-- DisambiguateRecordFields: the constructor determines the record, so the
-- field labels are legal unqualified even under a qualified-only import.
disambiguated :: Q.Box
disambiguated = Q.MkBox {content = myFunc 5, label = "x"}

inGuardAndWhere :: Int -> Int
inGuardAndWhere n
  | myFunc n > 10 = helper
  | otherwise = 0
  where
    helper = myOther n (sum [myFunc k | k <- [1 .. n]])

main :: IO ()
main = do
  print [direct, viaBacktick, qualifiedCall]
  print (partial <> [typeLevel disambiguated])
  print (inPattern disambiguated)
  print (inGuardAndWhere 6)
