module Mod (Box (..), myFunc, myOther) where

data Box = MkBox
  { content :: Int
  , label :: String
  }

myFunc :: Int -> Int
myFunc = (* 2)

myOther :: Int -> Int -> Int
myOther = (+)
