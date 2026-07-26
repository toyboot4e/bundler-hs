{-# LANGUAGE CPP #-}

-- Enables CPP but contains no # directives: parses as ordinary Haskell
-- and must bundle fine.
module Plain (answer) where

answer :: Int
answer = 41
