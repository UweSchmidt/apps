{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

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
       , tokenizeRE'
         -- Data.Maybe
       , fromMaybe
       , isNothing
       , isJust
       , listToMaybe
         -- Data.List
       , intercalate
       , isPrefixOf
       , isSuffixOf
       , partition
       , sort
       , sortBy
       , nub
         -- Data.Char
       , toLower
       , toUpper
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
       , IsoMaybe(..)
       , isoMapElems
       , isoMapList
       , isoSetList
       , isA
         -- utilities
       , (.||.)
       , partitionBy
       , searchPos
       )
where

import           Control.Arrow
import           Control.Lens
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import           Data.Char (toLower, toUpper)
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
{-# INLINE compareBy #-}


-- compare only on Just values
--
-- useful in compareBy when Nothing values occur

compareJust :: Ord a => Maybe a -> Maybe a -> Ordering
compareJust (Just x1) (Just x2) = compare x1 x2
compareJust _         _         = EQ
{-# INLINE compareJust #-}


-- compare with Nothing as largest values
--
-- default with compare: Nothing is smallest

compareJust' :: Ord a => Maybe a -> Maybe a -> Ordering
compareJust' (Just x1) (Just x2) = compare x1 x2
compareJust' (Just _ ) _         = LT
compareJust' _         (Just _ ) = GT
compareJust' _         _         = EQ
{-# INLINE compareJust' #-}

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
  {-# INLINE isempty #-}

instance IsEmpty (Maybe a) where
  isempty Nothing = True
  isempty _       = False
  {-# INLINE isempty #-}

instance IsEmpty Text where
  isempty = T.null
  {-# INLINE isempty #-}

instance IsEmpty ByteString where
  isempty = BS.null
  {-# INLINE isempty #-}

instance IsEmpty (Set a) where
  isempty = S.null
  {-# INLINE isempty #-}

instance IsEmpty (Map k v) where
  isempty = M.null
  {-# INLINE isempty #-}

-- ----------------------------------------

class IsoString a where
  isoString :: Iso' a String


instance IsoString [Char] where
  isoString = iso id id
  {-# INLINE isoString #-}

instance IsoString Text where
  isoString = iso T.unpack T.pack
  {-# INLINE isoString #-}

instance IsoString ByteString where
  isoString = iso BU.toString BU.fromString
  {-# INLINE isoString #-}

instance IsoString LazyByteString where
  isoString = iso LBU.toString LBU.fromString
  {-# INLINE isoString #-}

class IsoText a where
  isoText :: Iso' a Text

instance IsoText Text where
  isoText = iso id id
  {-# INLINE isoText #-}

class IsoInteger a where
  isoInteger :: Iso' a Integer

instance IsoInteger Integer where
  isoInteger = iso id id
  {-# INLINE isoInteger #-}

{-    (Use UndecidableInstances to permit this)
instance (Integral a) => IsoInteger a where
  isoInteger = iso toInteger fromInteger
-- -}

class IsoMaybe a where
  isoMaybe :: Iso' a (Maybe a)

-- here we need UndecidableInstances extension
-- AFAIK in this case it's not dangarous

instance (IsEmpty a, Monoid a) => IsoMaybe a where
  isoMaybe = iso toM fromM
    where
      toM xs
        | isempty xs = Nothing
        | otherwise  = Just xs
      fromM Nothing   = mempty
      fromM (Just xs) = xs
  {-# INLINE isoMaybe #-}

-- ----------------------------------------

-- an iso for converting a list of elemets into a map,
-- the key function extracts the keys of the elements

isoMapElems :: Ord k => (e -> k) -> Iso' (Map k e) [e]
isoMapElems key = iso M.elems (M.fromList . map (\ e -> (key e, e)))
{-# INLINE isoMapElems #-}

-- an iso for converting between maps and list of pairs

isoMapList :: Ord a => Iso' (Map a b) ([(a, b)])
isoMapList = iso M.toList M.fromList
{-# INLINE isoMapList #-}

isoSetList :: Ord a => Iso' (Set a) [a]
isoSetList = iso S.toList S.fromList
{-# INLINE isoSetList #-}

isA :: (a -> Bool) -> Prism' a a
isA p = prism id (\ o -> (if p o then Right else Left) o)
{-# INLINE isA #-}

{-
-- my first traversal, but Edward already made this, it's named each
-- works on various other structures

all3 :: Traversal (a, a, a) (b, b, b) a b
all3 inj (x1, x2, x3) = (,,) <$> inj x1 <*> inj x2 <*> inj x3
-- -}

-- ----------------------------------------
--
-- mothers little helper for en/decoding optional fileds

(.=?!) :: (ToJSON v, IsEmpty v) =>
          Text -> v -> [J.Pair]
t .=?! x
  | isempty x = []
  | otherwise = [t J..= x]
{-# INLINE (.=?!) #-}

(.:?!) :: (FromJSON v, Monoid v) =>
          J.Object -> Text -> J.Parser v
o .:?! t =
  o J..:? t J..!= mempty
{-# INLINE (.:?!) #-}

-- ----------------------------------------

infixr 2 .||.

-- | Lift boolean 'or' over predicates.

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p .||. q = \ v -> p v || q v


-- | group a list of entries by a mapping the
-- elements to an ordered domain

partitionBy :: (Ord e) => (a -> e) -> [a] -> [[a]]
partitionBy f =
  M.elems
  . foldr (\ x m -> M.insertWith (++) (f x) [x]
                    m) M.empty


-- search the position of the first element for which the predicate holds

searchPos :: (a -> Bool) -> [a] -> Maybe Int
searchPos p =
  listToMaybe . map fst . filter (p . snd) . zip [0..]

-- ----------------------------------------
