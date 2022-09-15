module Data.PriorityQueue.List
where

-- --------------------
--
-- a minimal priority queue

newtype PQueue c a = PQ [(c, a)]

emptyQ :: PQueue c a
emptyQ = PQ []

nullQ :: PQueue c a -> Bool
nullQ (PQ q) = null q

sizeQ :: PQueue c a -> Int
sizeQ (PQ xs) = length xs

splitMin :: PQueue c a -> (a, PQueue c a)
splitMin (PQ (x : xs)) = (snd x, PQ xs)
splitMin _             = error "splitMin: queue empty"

minElem :: PQueue c a -> a
minElem = fst . splitMin

deleteMin :: PQueue c a -> PQueue c a
deleteMin = snd . splitMin

insertQ :: Ord c => c -> a -> PQueue c a -> PQueue c a
insertQ k v (PQ q) = PQ $ ins q
  where
    ins []                  = [(k, v)]
    ins qs@(q1@(k1, _v1) : qs1)
      | k > k1              = q1     : ins qs1
      | otherwise           = (k, v) : qs

toListQ :: PQueue c a -> [(c, a)]
toListQ (PQ xs) = xs

deriving instance (Show c, Show a) => Show (PQueue c a)

-- --------------------
