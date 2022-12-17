module Data.PriorityQueue.Heap
  (PQueue, emptyQ, nullQ, sizeQ, splitMin, minElem, deleteMin, insertQ, toListQ)
where

-- --------------------
--
-- a minimal priority queue

data PQueue c a = E
                | N !c a (PQueue c a) (PQueue c a)

emptyQ :: PQueue c a
emptyQ = E

nullQ :: PQueue c a -> Bool
nullQ E = True
nullQ _ = False

sizeQ :: PQueue c a -> Int
sizeQ E = 0
sizeQ (N _c _a l r) = 1 + sizeQ l + sizeQ r

splitMin :: Ord c => PQueue c a -> (a, PQueue c a)
splitMin (N _c x l r) = (x, merge l r)
splitMin _            = error "splitMin: queue empty"

minElem :: Ord c => PQueue c a -> a
minElem = fst . splitMin

deleteMin :: Ord c => PQueue c a -> PQueue c a
deleteMin = snd . splitMin

insertQ :: Ord c => c -> a -> PQueue c a -> PQueue c a
insertQ c v q = merge (N c v E E) q

toListQ :: PQueue c a -> [(c, a)]
toListQ E = []
toListQ (N c a l r)  = (c, a) : toListQ l ++ toListQ r

deriving instance (Show c, Show a) => Show (PQueue c a)

merge :: Ord c => PQueue c a -> PQueue c a -> PQueue c a
merge E r = r
merge l E = l
merge l@(N c1 _ _ _) r@(N c2 _ _ _)
  | c1 <= c2  = join l r
  | otherwise = join r l

join :: Ord c => PQueue c a -> PQueue c a -> PQueue c a
join (N c1 x1 l1 r1) h2 = N c1 x1 r1 (merge l1 h2)
join _ _                = error "join: queue empty"

-- --------------------
