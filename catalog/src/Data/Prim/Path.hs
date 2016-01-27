{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Path
       ( Path
       , readPath
       , mkPath
       , consPath
       , showPath
       , path2string
       )
where

import Control.Lens (Iso', iso)
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import Data.Digest.Murmur64 (Hashable64(..))
import Data.String (IsString(..))
import Text.Regex.XMLSchema.Generic (tokenize)

import Data.Prim.Name

-- ----------------------------------------

data Path = BN !Name
          | DN !Name !Path

readPath :: String -> Path
readPath ('/' : xs0)
  = buildPath . tokenize "[^/]*" $ xs0
  where
    buildPath :: [String] -> Path
    buildPath [xs] = BN $ mkName xs
    buildPath (x : xs) = DN (mkName x) (buildPath xs)
    buildPath [] = error "buildPath: empty path"

readPath xs
  = error $ "readPath: no path: " ++ show xs

mkPath :: Name -> Path
mkPath = BN

consPath :: Name -> Path -> Path
consPath = DN

showPath :: Path -> String
showPath (BN n) = "/" ++ fromName n
showPath (DN n p) = "/" ++ fromName n ++ showPath p

path2string :: Iso' Path String
path2string = iso showPath readPath

deriving instance Eq  Path
deriving instance Ord Path

instance Show Path where
  show = showPath

instance ToJSON Path where
  toJSON = toJSON . showPath

instance FromJSON Path where
  parseJSON o = readPath <$> parseJSON o

instance IsString Path where
  fromString = readPath

instance Hashable64 Path where
  hash64Add = hash64Add . showPath

-- ----------------------------------------
