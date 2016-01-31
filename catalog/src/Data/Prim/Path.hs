{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Path
       ( Path'
       , Path
       , readPath
       , mkPath
       , emptyPath
       , nullPath
       , consPath
       , concPath
       , showPath
       , path2string
       )
where

import Control.Lens -- (Iso', iso)
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

readPath ""
  = emptyPath

readPath xs
  = error $ "readPath: no path: " ++ show xs

mkPath :: name -> Path' name
mkPath = BN

emptyPath :: Monoid name => Path' name
emptyPath = mkPath mempty

nullPath :: (Monoid name, Eq name) =>
            Path' name -> Bool
nullPath = (== emptyPath)

consPath :: (Monoid name, Eq name) =>
            name -> Path' name -> Path' name
consPath n p
  | n == mempty = p
  | nullPath p  = mkPath n
  | otherwise   = DN n p

concPath :: (Monoid name, Eq name) =>
            Path' name -> Path' name -> Path' name
concPath (BN n) p2    = consPath n p2
concPath (DN n p1) p2 = consPath n $ concPath p1 p2

showPath :: (Monoid name, Eq name, Show name) => Path' name -> String
showPath (BN n)
  | n == mempty   = ""
  | otherwise     = "/" ++ show n

showPath (DN n p) = "/" ++ show n ++ showPath p

path2string :: Iso' Path String
path2string = iso showPath readPath

deriving instance Eq name  => Eq  (Path' name)
deriving instance Ord name => Ord (Path' name)

-- deriving instance Show name => Show (Path' name)

instance (Eq name, Monoid name, Show name) => Show (Path' name) where
  show = showPath

instance (Eq name, Monoid name, Show name) => ToJSON (Path' name) where
  toJSON = toJSON . showPath

instance FromJSON Path where
  parseJSON o = readPath <$> parseJSON o

instance IsString Path where
  fromString = readPath

instance (Eq name, Monoid name, Show name) => Hashable64 (Path' name) where
  hash64Add = hash64Add . showPath

-- ----------------------------------------
