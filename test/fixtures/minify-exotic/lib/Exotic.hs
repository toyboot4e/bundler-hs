{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Exotic (Expr (..), eval, Container (..)) where

data Expr a where
  IntE :: Int -> Expr Int
  AddE :: Expr Int -> Expr Int -> Expr Int
  BoolE :: Bool -> Expr Bool

type family Elem c where
  Elem [a] = a
  Elem (Maybe a) = a

class Container f where
  type Idx f
  type Idx f = Int
  empty :: f a
  insert :: a -> f a -> f a

instance Container [] where
  empty = []
  insert = (:)

eval :: Expr a -> a
eval (IntE n) = n
eval (AddE a b) = eval a + eval b
eval (BoolE b) = b
