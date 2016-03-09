{-# LANGUAGE StandaloneDeriving #-}

-- paths used as object ids
-- not space efficient, but god for testing

module Data.Prim.PathId
       ( ObjId
       , mkObjId
       , objId2path
       )
       where

import Data.Prim.Path
import Data.Prim.Prelude

-- ----------------------------------------

newtype ObjId = ObjId Path

mkObjId :: Path -> ObjId
mkObjId = ObjId

showObjId :: ObjId -> String
showObjId (ObjId p) = show p

readObjId :: String -> ObjId
readObjId = ObjId . readPath

objId2path :: Iso' ObjId Path
objId2path = iso (\ (ObjId p) -> p) mkObjId

deriving instance Eq   ObjId
deriving instance Ord  ObjId

instance Monoid ObjId where
  mempty = ObjId mempty
  i1 `mappend` i2
    | isempty i1 = i2
    | otherwise    = i1

instance IsEmpty ObjId where
  isempty = (== mempty)

instance Show ObjId where
  show = showObjId

instance ToJSON ObjId where
  toJSON = toJSON . showObjId

instance FromJSON ObjId where
  parseJSON o = readObjId <$> parseJSON o

instance IsoString ObjId where
  isoString = iso showObjId readObjId

-- ----------------------------------------
