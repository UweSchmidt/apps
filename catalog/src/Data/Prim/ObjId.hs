{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.ObjId
where

import           Data.Word
import           Data.Maybe
import qualified Data.Digest.Murmur64 as MM
import           Control.Lens (Iso', iso)
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)

-- ----------------------------------------

newtype ObjId     = ObjId Word64

emptyObjId :: ObjId
emptyObjId = toObjId 0

nullObjId :: ObjId -> Bool
nullObjId (ObjId i) = i == 0

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
  iso (\ i -> if nullObjId i
              then Nothing
              else Just i
      )
      (fromMaybe emptyObjId)

deriving instance Eq   ObjId
deriving instance Ord  ObjId
deriving instance Show ObjId

instance ToJSON ObjId where
  toJSON = toJSON . fromObjId

instance FromJSON ObjId where
  parseJSON o = toObjId <$> parseJSON o

-- ----------------------------------------
