{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Name
where

import qualified Codec.Binary.UTF8.String as UTF8
import           Control.Lens (Iso', iso)
import           Control.Monad (mzero)
import qualified Data.Aeson as J
import           Data.Aeson (ToJSON, FromJSON, toJSON)
import qualified Data.ByteString as B
import qualified Data.List as L
import           Data.String(IsString(..))
import qualified Data.Text as T

-- ----------------------------------------
--
--names as UTF8 encoded strict bytestrings

newtype Name      = Name B.ByteString

emptyName :: Name
emptyName = mkName ""

mkName :: String -> Name
mkName = Name . B.pack . UTF8.encode

nullName :: Name -> Bool
nullName (Name n) = B.null n

fromName :: Name -> String
fromName (Name fsn) = UTF8.decode . B.unpack $ fsn

isNameSuffix :: Name -> Name -> Bool
isNameSuffix (Name sx) (Name n) = sx `B.isSuffixOf` n

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


deriving instance Eq   Name
deriving instance Ord  Name

instance Monoid Name where
  mempty = emptyName
  Name n1 `mappend` Name n2 = Name $ n1 `B.append` n2

instance IsString Name where
  fromString = mkName

instance Show Name where
  show = fromName

instance ToJSON Name where
  toJSON = toJSON . fromName

instance FromJSON Name where
  parseJSON (J.String t) = return (mkName . T.unpack $ t)
  parseJSON _            = mzero

-- ----------------------------------------
