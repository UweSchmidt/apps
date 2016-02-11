module Data.Function.Util where

import qualified Data.Map.Strict as M

-- ----------------------------------------

-- mothers little helpers

infixr 2 |||

-- | Lift boolean 'or' over predicates.
(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p ||| q = \ v -> p v || q v

-- | group a list of entries by a mapping the
-- elements to an ordered domain

groupBy :: (Ord e) => (a -> e) -> [a] -> [[a]]
groupBy f =
  M.elems
  . foldr (\ x m -> M.insertWith (++) (f x) [x]
                    m) M.empty


-- ----------------------------------------
