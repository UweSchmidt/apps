{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Name
       ( Name
       , mkName
       , isNameSuffix
       , substNameSuffix
       )
where

import           Data.Prim.Prelude
import           Control.Monad (mzero)
import qualified Data.Aeson as J
import qualified Data.List as L
import qualified Data.Text as T

-- ----------------------------------------
--
-- names as UTF8 encoded strict bytestrings
-- Text may be a good alternative

newtype Name = Name Text

emptyName :: Name
emptyName = mkName ""

mkName :: String -> Name
mkName = Name . T.pack

instance IsEmpty Name where
  isempty (Name n) = isempty n

fromName :: Name -> String
fromName (Name fsn) = T.unpack $ fsn

isNameSuffix :: Name -> Name -> Bool
isNameSuffix (Name sx) (Name n) = sx `T.isSuffixOf` n

substNameSuffix :: Name -> Name -> Name -> Name
substNameSuffix os' ns' n'
  | os `L.isSuffixOf` n =
      mkName . reverse . ((reverse ns) ++) . drop (length os) . reverse $ n
  | otherwise =
      n'
  where
    os = fromName os'
    ns = fromName ns'
    n  = fromName n'

deriving instance Eq   Name
deriving instance Ord  Name

instance IsoString Name where
  isoString = iso fromName mkName

instance IsoText Name where
  isoText = iso (\ (Name n) -> n) Name

instance Monoid Name where
  mempty = emptyName
  Name n1 `mappend` Name n2 = Name $ n1 `T.append` n2

instance IsString Name where
  fromString = mkName

instance Show Name where
  show = fromName

instance ToJSON Name where
  toJSON = toJSON . fromName

instance FromJSON Name where
  parseJSON (J.String t) = return (t ^. from isoText)
  parseJSON _            = mzero

-- ----------------------------------------
