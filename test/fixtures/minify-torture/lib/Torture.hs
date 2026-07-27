{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Torture
  ( pattern Single,
    Acc (..),
    (-->),
    readInt,
    sumStrict,
    classify,
    lazySnd,
  )
where

-- Explicitly bidirectional pattern synonym: builder equations live in a
-- where block.
pattern Single :: a -> [a]
pattern Single x <- [x]
  where
    Single x = [x]

-- Strict field: the ! must stay glued to the type.
data Acc = Acc {getAcc :: !Int}

-- An operator starting with the line-comment characters.
(-->) :: Bool -> Bool -> Bool
True --> b = b
False --> _ = True

infixr 1 -->

-- Type application: the @ must stay glued to the type.
readInt :: String -> Int
readInt = read @Int

-- Bang patterns in a multi-equation where helper: each ! must stay glued
-- to its variable.
sumStrict :: [Int] -> Int
sumStrict = go (Acc 0)
  where
    go (Acc !acc) [] = acc
    go (Acc !acc) (x : xs) = go (Acc (acc + x)) xs

-- Negative literal patterns.
classify :: Int -> String
classify (-1) = "neg one"
classify 0 = "zero"
classify _ = "other"

-- Lazy pattern.
lazySnd :: (Int, Int) -> Int
lazySnd ~(_, b) = b
