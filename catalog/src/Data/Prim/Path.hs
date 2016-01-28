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

data Path' name = BN !name
                | DN !name !(Path' name)

type Path = Path' Name

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

mkPath :: name -> Path' name
mkPath = BN

consPath :: name -> Path' name -> Path' name
consPath = DN

showPath :: Show name => Path' name -> String
showPath (BN n) = "/" ++ show n
showPath (DN n p) = "/" ++ show n ++ showPath p

path2string :: Iso' Path String
path2string = iso showPath readPath

deriving instance Eq name  => Eq  (Path' name)
deriving instance Ord name => Ord (Path' name)

instance Show name => Show (Path' name) where
  show = showPath

instance Show name => ToJSON (Path' name) where
  toJSON = toJSON . show

instance FromJSON Path where
  parseJSON o = readPath <$> parseJSON o

instance IsString Path where
  fromString = readPath

instance Show name => Hashable64 (Path' name) where
  hash64Add = hash64Add . show

-- ----------------------------------------
