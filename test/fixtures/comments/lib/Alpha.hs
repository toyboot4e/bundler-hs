module Alpha (Heap, emptyHeap, push, f, size) where

data Heap = Heap
  { items :: [Int]
  , size :: Int
  }

emptyHeap :: Heap
emptyHeap = Heap {items = [], size = 0}

push :: Int -> Heap -> Heap
push x h = Heap {items = x : items h, size = size h + 1}

-- Self-references: f uses the type, a field selector, and push's sibling.
f :: Heap -> Int
f h = sum (items h) + size h
