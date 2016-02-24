module Data.Prim.Prelude
       ( ByteString
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
       , matchSubexRE
         -- Data.Maybe
       , fromMaybe
         -- Data.List
       , intercalate
       , partition
         -- Control.Arrow
       , first, second, (&&&), (***)
       )
where

import Control.Arrow
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.ByteString (ByteString)
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Regex.XMLSchema.Generic

-- ----------------------------------------
