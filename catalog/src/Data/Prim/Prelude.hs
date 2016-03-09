{-# LANGUAGE RankNTypes #-}

module Data.Prim.Prelude
       ( ByteString
       , LazyByteString
       , Map
       , Set
       , Text
       , Vector
       , IsEmpty(..)
       , IsString(..)
         -- Data.Aeson
       , ToJSON(..)
       , FromJSON(..)
       , (.=?!)
       , (.:?!)
         -- Text.Regex.XMLSchema.Generic
       , Regex
       , RegexText
       , parseRegex
       , parseRegex'
       , parseRegexExt
       , parseRegexExt'
       , match
       , matchRE
       , matchSubex
       , matchSubexRE
         -- Data.Maybe
       , fromMaybe
       , isNothing
       , isJust
         -- Data.List
       , intercalate
       , isPrefixOf
       , isSuffixOf
       , partition
       , sort
       , sortBy
       , nub
         -- Data.Function
       , on
         -- System.FilePath
       , FilePath
       , (</>)
       , takeFileName
       , takeDirectory
         -- Control.Arrow
       , first, second, (&&&), (***)
         -- this module
       , compareBy
       , compareJust
       , compareJust'
       , partBy
         -- lens stuff
       , module Control.Lens
       , IsoString(..)
       , IsoText(..)
       , IsoInteger(..)
       , isoMapElems
       , isoMapList
       , isoSetList
       , isoTextMaybe
       , isoMonoidMaybe
       , isA
       )
where

import           Control.Arrow
import           Control.Lens
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import           System.FilePath
import           Text.Regex.XMLSchema.Generic
import           Text.Regex.XMLSchema.Generic.RegexParser

type LazyByteString = LB.ByteString

-- ----------------------------------------

compareBy :: [a -> a -> Ordering] -> a -> a -> Ordering
compareBy fs x1 x2 =
  mconcat $ map (\ cmp -> cmp x1 x2) fs


-- compare only on Just values
--
-- useful in compareBy when Nothing values occur

compareJust :: Ord a => Maybe a -> Maybe a -> Ordering
compareJust (Just x1) (Just x2) = compare x1 x2
compareJust _         _         = EQ


-- compare with Nothing as largest values
--
-- default with compare: Nothing is smallest

compareJust' :: Ord a => Maybe a -> Maybe a -> Ordering
compareJust' (Just x1) (Just x2) = compare x1 x2
compareJust' (Just _ ) _         = LT
compareJust' _         (Just _ ) = GT
compareJust' _         _         = EQ

-- ----------------------------------------

-- put all elemnts of a, which have equal e values
-- into a sublist
--
-- partBy (`mod` 3) [0..9] = [[0,3,6,9],[1,4,7],[2,5,8]]

partBy :: (Ord e) => (a -> e) -> [a] -> [[a]]
partBy f =
  M.elems
  . foldr (\ x m -> M.insertWith (++) (f x) [x]
                    m) M.empty

-- ----------------------------------------

class IsEmpty a where
  isempty :: a -> Bool


instance IsEmpty [a] where
  isempty = null

instance IsEmpty (Maybe a) where
  isempty Nothing = True
  isempty _       = False

instance IsEmpty Text where
  isempty = T.null

instance IsEmpty ByteString where
  isempty = BS.null

instance IsEmpty (Set a) where
  isempty = S.null

instance IsEmpty (Map k v) where
  isempty = M.null

-- ----------------------------------------

class IsoString a where
  isoString :: Iso' a String


instance IsoString [Char] where
  isoString = iso id id

instance IsoString Text where
  isoString = iso T.unpack T.pack

instance IsoString ByteString where
  isoString = iso BU.toString BU.fromString

instance IsoString LazyByteString where
  isoString = iso LBU.toString LBU.fromString

class IsoText a where
  isoText :: Iso' a Text

instance IsoText Text where
  isoText = iso id id

class IsoInteger a where
  isoInteger :: Iso' a Integer

instance IsoInteger Integer where
  isoInteger = iso id id

{-    (Use UndecidableInstances to permit this)
instance (Integral a) => IsoInteger a where
  isoInteger = iso toInteger fromInteger
-- -}

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

isoTextMaybe :: Iso' Text (Maybe Text)
isoTextMaybe =
  iso (\ t -> if T.null t then Nothing else Just t)
      (fromMaybe T.empty)

isoMonoidMaybe :: (Monoid a, Eq a) => Iso' a (Maybe a)
isoMonoidMaybe =
  iso (\ t -> if t == mempty then Nothing else Just t)
      (fromMaybe mempty)

-- a prism for filtering

isA :: (a -> Bool) -> Prism' a a
isA p = prism id (\ o -> (if p o then Right else Left) o)

-- ----------------------------------------
--
-- mothers little helper for en/decoding optional fileds

(.=?!) :: (ToJSON v, IsEmpty v) =>
          Text -> v -> [J.Pair]
t .=?! x
  | isempty x = []
  | otherwise = [t J..= x]

(.:?!) :: (FromJSON v, Monoid v) =>
          J.Object -> Text -> J.Parser v
o .:?! t =
  o J..:? t J..!= mempty

-- ----------------------------------------
