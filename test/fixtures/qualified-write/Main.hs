module Main (main) where

-- An unqualified import allows qualified access too, and writing
-- Deque.push names the module just as `import qualified` would. That
-- settles it for the whole module, so `pop` moves as well.
import Deque

main :: IO ()
main = print (Deque.push 1 (pop [2, 3]))
