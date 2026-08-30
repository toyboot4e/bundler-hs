module Main (main) where

import qualified Greet

-- 日本語のコメント: 挨拶を表示する
main :: IO ()
main = putStrLn (Greet.greeting <> "、世界")
