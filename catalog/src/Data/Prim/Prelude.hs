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
       , parseRegex
       , parseRegexExt
       , match
       , matchRE
       , matchSubex
       , matchSubexRE
         -- Data.Maybe
       , fromMaybe
         -- Data.List
       , intercalate
       , isPrefixOf
       , isSuffixOf
       , partition
         -- System.FilePath
       , FilePath
       , (</>)
       , takeFileName
       , takeDirectory
         -- Control.Arrow
       , first, second, (&&&), (***)
       )
where

import Control.Arrow
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Vector (Vector)
import System.FilePath
import Text.Regex.XMLSchema.Generic

type LazyByteString = LB.ByteString

-- ----------------------------------------
