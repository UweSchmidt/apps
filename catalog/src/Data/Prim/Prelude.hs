module Data.Prim.Prelude
       ( ByteString
       , LazyByteString
       , Map
       , Set
       , Text
       , Vector
         -- Data.Aeson
       , ToJSON(..)
       , FromJSON(..)
       , IsString(..)
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
       )
where

import           Control.Arrow
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Set (Set)
import           Data.String (IsString(..))
import           Data.Text (Text)
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
