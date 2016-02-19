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
       , snocPath
       , tailPath
       , headPath
       , substPathName
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

data Path' n = BN !n
                | DN !n !(Path' n)

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

mkPath :: n -> Path' n
mkPath = BN

emptyPath :: Monoid n => Path' n
emptyPath = mkPath mempty

nullPath :: (Monoid n, Eq n) =>
            Path' n -> Bool
nullPath = (== emptyPath)

consPath :: (Monoid n, Eq n) =>
            n -> Path' n -> Path' n
consPath n p
  | n == mempty = p
  | nullPath p  = mkPath n
  | otherwise   = DN n p

snocPath :: (Monoid n, Eq n) =>
            Path' n -> n -> Path' n
snocPath p n = p `concPath` mkPath n

concPath :: (Monoid n, Eq n) =>
            Path' n -> Path' n -> Path' n
concPath (BN n) p2    = consPath n p2
concPath (DN n p1) p2 = consPath n $ concPath p1 p2

substPathName :: n -> Path' n -> Path' n
substPathName n (BN _)    = BN n
substPathName n (DN n' p) = DN n' (substPathName n p)

headPath :: Path' n -> n
headPath (DN n _p) = n
headPath (BN n)    = n

tailPath :: Path' n -> Path' n
tailPath (DN _n p) = p
tailPath p         = p

showPath :: (Monoid n, Eq n, Show n) => Path' n -> String
showPath (BN n)
  | n == mempty   = ""
  | otherwise     = "/" ++ show n

showPath (DN n p) = "/" ++ show n ++ showPath p

path2string :: Iso' Path String
path2string = iso showPath readPath

deriving instance Eq n  => Eq  (Path' n)
deriving instance Ord n => Ord (Path' n)

-- deriving instance Show n => Show (Path' n)

instance (Eq n, Monoid n, Show n) => Show (Path' n) where
  show = showPath

instance (Eq n, Monoid n, Show n) => ToJSON (Path' n) where
  toJSON = toJSON . showPath

instance FromJSON Path where
  parseJSON o = readPath <$> parseJSON o

instance IsString Path where
  fromString = readPath

instance (Eq n, Monoid n, Show n) => Hashable64 (Path' n) where
  hash64Add = hash64Add . showPath

-- ----------------------------------------
