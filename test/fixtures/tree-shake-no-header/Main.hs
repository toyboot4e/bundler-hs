-- No module header at all, so there is no export list to shake against:
-- the file is Main and main is the only way in.
import Algo (solve)

unreachable :: Int
unreachable = 99

main :: IO ()
main = print (solve [1, 2, 3])
