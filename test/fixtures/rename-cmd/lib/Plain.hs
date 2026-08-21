-- Imported unqualified and claimed by nobody, so the default rule would
-- leave `plain` alone: the rename command is told an empty suffix.
module Plain (plain) where

plain :: Int -> Int
plain = negate
