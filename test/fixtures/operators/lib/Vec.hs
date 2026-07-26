module Vec ((<+>), origin, scale) where

infixl 6 <+>

type V2 = (Int, Int)

origin :: V2
origin = (0, 0)

(<+>) :: V2 -> V2 -> V2
(a, b) <+> (c, d) = (a + c, b + d)

scale :: Int -> V2 -> V2
scale k v = (k, k) `mul` v
  where
    mul (a, b) (c, d) = (a * c, b * d)
