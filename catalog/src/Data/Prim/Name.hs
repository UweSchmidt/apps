{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Name
where

import           Data.Prim.Prelude
import           Control.Lens (Iso', iso, from, (^.))
import           Control.Monad (mzero)
import qualified Data.Aeson as J
import qualified Data.List as L
import qualified Data.Text as T

-- ----------------------------------------
--
-- names as UTF8 encoded strict bytestrings
-- Text may be a good alternative

newtype Name      = Name Text

emptyName :: Name
emptyName = mkName ""

mkName :: String -> Name
mkName = Name . T.pack

nullName :: Name -> Bool
nullName (Name n) = T.null n

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

name2string :: Iso' Name String
name2string = iso fromName mkName

name2text :: Iso' Name Text
name2text = iso (\ (Name n) -> n) Name

deriving instance Eq   Name
deriving instance Ord  Name

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
  parseJSON (J.String t) = return (t ^. from name2text)
  parseJSON _            = mzero

-- ----------------------------------------
