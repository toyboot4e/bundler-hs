module Beta (f) where

-- Same name as Alpha.f: both must survive in the bundle under
-- different names.
f :: Int -> Int
f = succ
