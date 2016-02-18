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
       , mkEmptyImgRoot
       , theParts
       , theDirTimeStamp
       , theDirEntries
       , emptyImg
       , emptyImgDir
       , emptyImgRoot
       , emptyImgCol
       , theImgDir
       , theImgRoot
       , isImgCol
       , isDIR
       , isIMG
       , isROOT
       , isCOL
       , nullImgDir
       , mkImgRoot
       , mkImgNode
       , remImgNode
       , emptyImgParts
       , mkImgParts
       , isoImgParts
       , mkImgPart
       , theImgName
       , theImgType
       , theImgTimeStamp
       , theImgCheckSum
       , theRootImgDir
       , theRootImgCol
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
import           Data.Prim.CheckSum
import           Data.Prim.Name
import           Data.Prim.PathId -- change this to ObjId later
import           Data.Prim.TimeStamp
import           Data.RefTree
import           Data.Set (Set)
import qualified Data.Set as S
-- import           Data.Maybe
--import           Data.Prim.Path

-- ----------------------------------------


data ImgNode' ref = IMG  !ImgParts
                  | DIR  !TimeStamp !(Set ref)
                  | ROOT !ref !ref
                  | COL

deriving instance (Show ref) => Show (ImgNode' ref)

instance ToJSON ref => ToJSON (ImgNode' ref) where
  toJSON (IMG pm) = object
    [ "ImgNode"     .= ("IMG" :: String)
    , "parts"       .= pm
    ]
  toJSON (DIR ts rs) = object
    [ "ImgNode"     .= ("DIR" :: String)
    , "timestamp"   .= ts
    , "children"    .= S.toList rs
    ]
  toJSON (ROOT rd rc) = object
    [ "ImgNode"     .= ("ROOT" :: String)
    , "archive"     .= rd
    , "collections" .= rc
    ]
  toJSON (COL) = object
    [ "ImgNode"    .= ("COL" :: String)
    ]

instance (Ord ref, FromJSON ref) => FromJSON (ImgNode' ref) where
  parseJSON = withObject "ImgNode" $ \ o ->
    do t <- o .: "ImgNode"
       case t :: String of
         "IMG" ->
           IMG <$> o .: "parts"
         "DIR" ->
           DIR <$> o .: "timestamp"
               <*> (S.fromList <$> o .: "children")
         "ROOT" ->
           ROOT <$> o .: "archive"
                <*> o .: "collections"
         "COL" ->
           return COL
         _ -> mzero

emptyImgDir :: ImgNode' ref
emptyImgDir = DIR zeroTimeStamp S.empty

emptyImg :: ImgNode' ref
emptyImg = IMG emptyImgParts

emptyImgRoot :: Monoid ref => ImgNode' ref
emptyImgRoot = ROOT mempty mempty

emptyImgCol :: ImgNode' ref
emptyImgCol = COL

-- image node optics

theParts :: Prism' (ImgNode' ref) ImgParts
theParts
  = prism IMG (\ x -> case x of
                  IMG m -> Right m
                  _     -> Left x
              )

theImgDir :: Prism' (ImgNode' ref) (TimeStamp, Set ref)
theImgDir
  = prism (uncurry DIR)
          (\ x -> case x of
              DIR ts s -> Right (ts, s)
              _        -> Left x
          )

theDirTimeStamp :: Traversal' (ImgNode' ref) TimeStamp
theDirTimeStamp = theImgDir . _1

theDirEntries ::  Traversal' (ImgNode' ref) (Set ref)
theDirEntries = theImgDir . _2

theImgRoot :: Prism' (ImgNode' ref) (ref, ref)
theImgRoot
  = prism (uncurry ROOT)
          (\ x -> case x of
              ROOT rd rc -> Right (rd, rc)
              _          -> Left x
          )

theRootImgDir :: Traversal' (ImgNode' ref) ref
theRootImgDir = theImgRoot . _1

theRootImgCol :: Traversal' (ImgNode' ref) ref
theRootImgCol = theImgRoot . _2

isImgCol :: Prism' (ImgNode' ref) ()
isImgCol
  = prism (const COL)
          (\ x -> case x of
              COL -> Right ()
              _   -> Left x
          )

isDIR :: ImgNode' ref -> Bool
isDIR DIR{}  = True
isDIR _      = False

isIMG :: ImgNode' ref -> Bool
isIMG IMG{}  = True
isIMG _      = False

isROOT :: ImgNode' ref -> Bool
isROOT ROOT{} = True
isROOT _      = False

isCOL :: ImgNode' ref -> Bool
isCOL COL{} = True
isCOL _     = False

nullImgDir :: ImgNode' ref -> Bool
nullImgDir (DIR _ s) = S.null s
nullImgDir _         = True

-- ----------------------------------------

-- the tree for the image hierachy

type ImgTree = DirTree ImgNode' ObjId
type ImgNode = ImgNode' ObjId

mkEmptyImgRoot :: (MonadError String m) =>
                  Name -> Name -> Name -> m ImgTree
mkEmptyImgRoot rootName imgName colName =
  do (_r1,t1) <- mkDirNode mkObjId isROOT addImgArchive imgName r emptyImgDir t0
     (_r2,t2) <- mkDirNode mkObjId isROOT addImgCol     colName r emptyImgCol t1
     return t2
  where
    t0 = mkDirRoot mkObjId rootName emptyImgRoot
    r  = t0 ^. rootRef

    addImgArchive r' n = n & theRootImgDir .~ r'
    addImgCol     r' n = n & theRootImgCol .~ r'

mkImgRoot :: Name -> ImgNode -> ImgTree
mkImgRoot = mkDirRoot mkObjId

mkImgNode :: (MonadError String m) =>
             Name ->                       -- name of the node
             ObjId ->                      -- parent node
             ImgNode ->                    -- node value
             ImgTree -> m (ObjId, ImgTree) -- new ref and modified tree
mkImgNode = mkDirNode mkObjId isDIR addChildRef

remImgNode :: (MonadError String m) =>
              ObjId ->
              ImgTree -> m ImgTree
remImgNode = remDirNode nullImgDir remChildRef

addChildRef :: ObjId -> ImgNode -> ImgNode
addChildRef r n = n & theDirEntries %~ S.insert r

remChildRef :: ObjId -> ImgNode -> ImgNode
remChildRef r n = n & theDirEntries %~ S.delete r

-- ----------------------------------------

newtype ImgParts = ImgParts (Map Name ImgPart)

deriving instance Show ImgParts

instance Monoid ImgParts where
  mempty = emptyImgParts

  ImgParts m1 `mappend` ImgParts m2
    = ImgParts $ M.mergeWithKey combine only1 only2 m1 m2
    where
      only1 = const M.empty
      only2 = id
      combine _k e1 e2
        | t1 >= t2  = Just e1
        | otherwise = Just e2
        where
          t1 = e1 ^. theImgTimeStamp
          t2 = e2 ^. theImgTimeStamp

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

mkImgPart :: Name -> ImgType -> ImgPart
mkImgPart n t = IP n t zeroTimeStamp zeroCheckSum

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
