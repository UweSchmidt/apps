{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.ImageTree
       ( ImgNode', ImgNode
       , ImgTree
       , ImgParts
       , ImgPart
       , ImgType(..)
       , theParts
       , theDirTimeStamp
       , theDirEntries
       , isImgDir
       , mkImgNode
       , remImgNode
       , emptyImgParts
       , mkImgParts
       , isoImgParts
       , theImgName
       , theImgType
       , theImgTimeStamp
       , theImgCheckSum
       )
where

import           Control.Lens hiding ((.=))
import           Control.Lens.Util
import           Control.Monad.Except

import           Data.Aeson (ToJSON(..), FromJSON(..)
                            , (.:), (.=)
                            , object, withObject
                            )
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
-- import           Data.Maybe
import           Data.Prim.Name
import           Data.Prim.CheckSum
--import           Data.Prim.Path
import           Data.Prim.PathId -- change this to ObjId later
import           Data.Prim.TimeStamp
import           Data.RefTree
import           Data.Set (Set)
import qualified Data.Set as S

-- ----------------------------------------


data ImgNode' ref = IMG !(Map Name ImgParts)
                  | DIR !TimeStamp !(Set ref)

deriving instance (Show ref) => Show (ImgNode' ref)

instance ToJSON ref => ToJSON (ImgNode' ref) where
  toJSON (IMG pm) = object
    [ "ImgNode" .= ("IMG" :: String)
    , "parts"   .= M.toList pm
    ]
  toJSON (DIR ts rs) = object
    [ "ImgNode"   .= ("DIR" :: String)
    , "timestamp" .= ts
    , "children"  .= S.toList rs
    ]

instance (Ord ref, FromJSON ref) => FromJSON (ImgNode' ref) where
  parseJSON = withObject "ImgNode" $ \ o ->
    do t <- o .: "ImgNode"
       case t :: String of
         "IMG" ->
           IMG . M.fromList
               <$> o .: "parts"
         "DIR" ->
           DIR <$> o .: "timestamp"
               <*> (S.fromList <$> o .: "children")
         _ -> mzero

-- image node optics

theParts :: Prism' (ImgNode' ref) (Map Name ImgParts)
theParts
  = prism IMG (\ x -> case x of
                  IMG m -> Right m
                  _     -> Left x
              )

isImgDir :: Prism' (ImgNode' ref) (TimeStamp, Set ref)
isImgDir
  = prism (uncurry DIR)
          (\ x -> case x of
              DIR ts s -> Right (ts, s)
              _        -> Left x
          )

theDirTimeStamp :: Traversal' (ImgNode' ref) TimeStamp
theDirTimeStamp = isImgDir . _1

theDirEntries ::  Traversal' (ImgNode' ref) (Set ref)
theDirEntries = isImgDir . _2

-- ----------------------------------------

-- the tree for the image hierachy

type ImgTree = DirTree ImgNode' ObjId
type ImgNode = ImgNode' ObjId


mkImgNode :: (MonadError String m) =>
             Name ->                       -- name of the node
             ObjId ->                      -- parent node
             ImgNode ->                    -- node value
             ImgTree -> m (ObjId, ImgTree) -- new ref and modified tree
mkImgNode = mkDirNode mkObjId isParentDir addChildRef

remImgNode :: (MonadError String m) =>
              ObjId ->
              ImgTree -> m ImgTree
remImgNode = remDirNode nullImgDir remChildRef

addChildRef :: ObjId -> ImgNode -> ImgNode
addChildRef r n = n & theDirEntries %~ S.insert r

remChildRef :: ObjId -> ImgNode -> ImgNode
remChildRef r n = n & theDirEntries %~ S.delete r

isParentDir :: ImgNode -> Bool
isParentDir DIR{} = True
isParentDir _     = False

nullImgDir :: ImgNode -> Bool
nullImgDir (DIR _ s) = S.null s
nullImgDir _         = True

-- ----------------------------------------

newtype ImgParts = ImgParts (Map Name ImgPart)

deriving instance Show ImgParts

instance ToJSON ImgParts where
  toJSON (ImgParts pm) = toJSON . M.toList $ pm

instance FromJSON ImgParts where
  parseJSON x = (ImgParts . M.fromList) <$> parseJSON x

emptyImgParts :: ImgParts
emptyImgParts = ImgParts M.empty

mkImgParts :: [ImgPart] -> ImgParts
mkImgParts ps = ps ^. from isoImgParts

isoImgParts :: Iso' ImgParts [ImgPart]
isoImgParts =
  iso (\ (ImgParts pm) -> pm) ImgParts
  .
  isoMapElems (\ (IP n _ _ _) -> n)

-- ----------------------------------------

data ImgPart     = IP !Name !ImgType !TimeStamp !CheckSum

deriving instance Show ImgPart

instance ToJSON ImgPart where
  toJSON (IP n t s c) = object
    [ "Name"      .= n
    , "ImgType"   .= t
    , "TimeStamp" .= s
    , "CheckSum"  .= c
    ]

instance FromJSON ImgPart where
  parseJSON = withObject "ImgPart" $ \ o ->
    IP <$> o .: "Name"
       <*> o .: "ImgType"
       <*> o .: "TimeStamp"
       <*> o .: "CheckSum"

theImgName :: Lens' ImgPart Name
theImgName k (IP n t s c) = (\ new -> IP new t s c) <$> k n

theImgType :: Lens' ImgPart ImgType
theImgType k (IP n t s c) = (\ new -> IP n new s c) <$> k t

theImgTimeStamp :: Lens' ImgPart TimeStamp
theImgTimeStamp k (IP n t s c) = (\ new -> IP n t new c) <$> k s

theImgCheckSum :: Lens' ImgPart CheckSum
theImgCheckSum k (IP n t s c) = (\ new -> IP n t s new) <$> k c

-- ----------------------------------------

data ImgType     = IMGraw    | IMGmeta   | IMGjson  | IMGjpg | IMGimg | IMGcopy
                 | IMGimgdir | IMGjpgdir | IMGother | IMGboring

deriving instance Eq   ImgType
deriving instance Ord  ImgType
deriving instance Show ImgType
deriving instance Read ImgType

instance ToJSON ImgType where
  toJSON = toJSON . show

instance FromJSON ImgType where
  parseJSON o = read <$> parseJSON o

-- ----------------------------------------
