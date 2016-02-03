{-# LANGUAGE RankNTypes #-}

module Control.Lens.Util where

import           Control.Lens
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S

-- ----------------------------------------

-- an iso for converting a list of elemets into a map,
-- the key function extracts the keys of the elements

isoMapElems :: Ord k => (e -> k) -> Iso' (Map k e) [e]
isoMapElems key = iso M.elems (M.fromList . map (\ e -> (key e, e)))

-- an iso for converting between maps and list of pairs

isoMapList :: Ord a => Iso' (Map a b) ([(a, b)])
isoMapList = iso M.toList M.fromList

isoSetList :: Ord a => Iso' (Set a) [a]
isoSetList = iso S.toList S.fromList

-- a prism for filtering

is :: (a -> Bool) -> Prism' a a
is p = prism id (\ o -> (if p o then Right else Left) o)

-- ----------------------------------------
