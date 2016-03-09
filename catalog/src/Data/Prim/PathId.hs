{-# LANGUAGE StandaloneDeriving #-}

-- paths used as object ids
-- not space efficient, but god for testing

module Data.Prim.PathId
where

import Data.Maybe
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

objId2string :: Iso' ObjId String
objId2string = iso showObjId readObjId

objId2Maybe :: Iso' ObjId (Maybe ObjId)
objId2Maybe =
  iso (\ i -> if isempty i
              then Nothing
              else Just i
      )
      (fromMaybe mempty)

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

-- ----------------------------------------
