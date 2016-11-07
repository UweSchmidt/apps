{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.ObjId
where

import qualified Data.Digest.Murmur64 as MM
import           Data.Maybe
import           Data.Prim.Prelude
import           Data.Word

-- ----------------------------------------

newtype ObjId     = ObjId Word64

mkObjId :: MM.Hashable64 a => a -> ObjId
mkObjId = ObjId . MM.asWord64 . MM.hash64
{-# INLINE mkObjId #-}

fromObjId :: ObjId -> Integer
fromObjId (ObjId w) = fromIntegral w
{-# INLINE fromObjId #-}

toObjId :: Integer -> ObjId
toObjId = ObjId . fromIntegral
{-# INLINE toObjId #-}

objId2integer :: Iso' ObjId Integer
objId2integer = iso fromObjId toObjId
{-# INLINE objId2integer #-}

objId2Int :: Iso' ObjId Int
objId2Int = iso (\ (ObjId w) -> fromIntegral w)
                (ObjId . fromIntegral)
{-# INLINE objId2Int #-}

objId2Maybe :: Iso' ObjId (Maybe ObjId)
objId2Maybe =
  iso (\ i -> if isempty i
              then Nothing
              else Just i
      )
      (fromMaybe mempty)
{-# INLINE objId2Maybe #-}


deriving instance Eq   ObjId
deriving instance Ord  ObjId
deriving instance Show ObjId

instance Monoid ObjId where
  mempty = toObjId 0

  i1 `mappend` i2
    | isempty i1 = i2
    | otherwise  = i1
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}


instance IsEmpty ObjId where
  isempty = (== mempty)
  {-# INLINE isempty #-}

instance ToJSON ObjId where
  toJSON = toJSON . fromObjId
  {-# INLINE toJSON #-}

instance FromJSON ObjId where
  parseJSON o = toObjId <$> parseJSON o
  {-# INLINE parseJSON #-}

instance IsoString ObjId where
  isoString = objId2integer . isoString
  {-# INLINE isoString #-}

-- ----------------------------------------
