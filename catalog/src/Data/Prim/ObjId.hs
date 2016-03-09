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

fromObjId :: ObjId -> Integer
fromObjId (ObjId w) = fromIntegral w

toObjId :: Integer -> ObjId
toObjId = ObjId . fromIntegral

objId2integer :: Iso' ObjId Integer
objId2integer = iso fromObjId toObjId

objId2Maybe :: Iso' ObjId (Maybe ObjId)
objId2Maybe =
  iso (\ i -> if isempty i
              then Nothing
              else Just i
      )
      (fromMaybe mempty)

deriving instance Eq   ObjId
deriving instance Ord  ObjId
deriving instance Show ObjId

instance Monoid ObjId where
  mempty = toObjId 0
  i1 `mappend` i2
    | isempty i1 = i2
    | otherwise  = i1

instance IsEmpty ObjId where
  isempty = (== mempty)

instance ToJSON ObjId where
  toJSON = toJSON . fromObjId

instance FromJSON ObjId where
  parseJSON o = toObjId <$> parseJSON o

-- ----------------------------------------
