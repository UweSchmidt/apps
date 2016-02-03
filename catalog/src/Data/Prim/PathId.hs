{-# LANGUAGE StandaloneDeriving #-}

-- paths used as object ids
-- not space efficient, but god for testing

module Data.Prim.PathId
where

import Data.Maybe
import Control.Lens -- (Iso', iso)
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import Data.Prim.Path

-- ----------------------------------------

newtype ObjId = ObjId Path

emptyObjId :: ObjId
emptyObjId = ObjId emptyPath

nullObjId :: ObjId -> Bool
nullObjId (ObjId i) = nullPath i

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
  iso (\ i -> if nullObjId i
              then Nothing
              else Just i
      )
      (fromMaybe emptyObjId)

deriving instance Eq   ObjId
deriving instance Ord  ObjId

instance Monoid ObjId where
  mempty = emptyObjId
  i1 `mappend` i2
    | nullObjId i1 = i2
    | otherwise    = i1

instance Show ObjId where
  show = showObjId

instance ToJSON ObjId where
  toJSON = toJSON . showObjId

instance FromJSON ObjId where
  parseJSON o = readObjId <$> parseJSON o

-- ----------------------------------------
