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
       , NameImgType
       , ColEntry
       , mkEmptyImgRoot
       , mkImgRoot
       , mkImgNode
       , mkImgParts
       , mkImgPart
       , emptyImg
       , emptyImgDir
       , emptyImgRoot
       , emptyImgCol
       , emptyImgParts
       , isDIR
       , isIMG
       , isROOT
       , isCOL
       , nullImgDir
       , isoImgParts
       , theParts
       , theImgName
       , theImgType
       , theImgTimeStamp
       , theImgCheckSum
       , theDir
       , theDirEntries
       , theDirSyncTime
       , theRootImgDir
       , theRootImgCol
       , theImgRoot
       , theImgCol
       , theColMetaData
       , theColEntries
       , theColSyncTime
       , theColColRef
       , theColImgRef
       , removeImgNode
       )
where

import           Control.Lens hiding ((.=))
import           Control.Lens.Util
import           Control.Monad.Except
import           Data.Aeson ( (.:), (.=), (.:?), (.!=)
                            , object, withObject
                            )
import qualified Data.Map.Strict as M
import           Data.MetaData
import           Data.Prim.CheckSum
import           Data.Prim.Name
import           Data.Prim.PathId -- change this to ObjId later
import           Data.Prim.Prelude
import           Data.Prim.TimeStamp
import           Data.RefTree
import qualified Data.Set as S

-- ----------------------------------------


data ImgNode' ref = IMG  !ImgParts
                  | DIR  !(Set ref) !TimeStamp
                  | ROOT !ref !ref
                  | COL  !MetaData ![ColEntry] !TimeStamp

-- ----------------------------------------

deriving instance (Show ref) => Show (ImgNode' ref)

instance ToJSON ref => ToJSON (ImgNode' ref) where
  toJSON (IMG pm) = object
    [ "ImgNode"     .= ("IMG" :: String)
    , "parts"       .= pm
    ]
  toJSON (DIR rs ts) = object
    [ "ImgNode"     .= ("DIR" :: String)
    , "children"    .= S.toList rs
    , "sync"        .= ts
    ]
  toJSON (ROOT rd rc) = object
    [ "ImgNode"     .= ("ROOT" :: String)
    , "archive"     .= rd
    , "collections" .= rc
    ]
  toJSON (COL md es ts) = object
    [ "ImgNode"    .= ("COL" :: String)
    , "metadata"   .= md
    , "entries"    .= es
    , "sync"       .= ts
    ]

instance (Ord ref, FromJSON ref) => FromJSON (ImgNode' ref) where
  parseJSON = withObject "ImgNode" $ \ o ->
    do t <- o .: "ImgNode"
       case t :: String of
         "IMG" ->
           IMG  <$> o .: "parts"
         "DIR" ->
           DIR  <$> (S.fromList <$> o .: "children")
                <*> o .:? "sync" .!= zeroTimeStamp
         "ROOT" ->
           ROOT <$> o .: "archive"
                <*> o .: "collections"
         "COL" ->
           COL  <$> o .: "metadata"
                <*> o .: "entries"
                <*> o .: "sync"
         _ -> mzero

emptyImgDir :: ImgNode' ref
emptyImgDir = DIR S.empty zeroTimeStamp

emptyImg :: ImgNode' ref
emptyImg = IMG emptyImgParts

emptyImgRoot :: Monoid ref => ImgNode' ref
emptyImgRoot = ROOT mempty mempty

emptyImgCol :: ImgNode' ref
emptyImgCol = COL emptyMetaData [] zeroTimeStamp

-- image node optics

theParts :: Prism' (ImgNode' ref) ImgParts
theParts
  = prism IMG (\ x -> case x of
                  IMG m -> Right m
                  _     -> Left  x
              )

theDir :: Prism' (ImgNode' ref) (Set ref, TimeStamp)
theDir =
  prism (uncurry DIR)
        (\ x -> case x of
                DIR s t -> Right (s, t)
                _       -> Left  x
          )

theDirEntries :: Traversal' (ImgNode' ref) (Set ref)
theDirEntries = theDir . _1

theDirSyncTime :: Traversal' (ImgNode' ref) TimeStamp
theDirSyncTime = theDir . _2

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

theImgCol :: Prism' (ImgNode' ref) (MetaData, [ColEntry], TimeStamp)
theImgCol
  = prism (\ (x1, x2, x3) -> COL x1 x2 x3)
          (\ x -> case x of
              COL x1 x2 x3 -> Right (x1, x2, x3)
              _            -> Left x
          )

theColMetaData :: Traversal' (ImgNode' ref) MetaData
theColMetaData = theImgCol . _1

theColEntries :: Traversal' (ImgNode' ref) [ColEntry]
theColEntries = theImgCol . _2

theColSyncTime :: Traversal' (ImgNode' ref) TimeStamp
theColSyncTime = theImgCol . _3

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
nullImgDir (DIR s _) = S.null s
nullImgDir _       = True

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

-- | remove an image node or a dir node without entries
removeImgNode :: (MonadError String m) =>
                 ObjId ->
                 ImgTree -> m ImgTree
removeImgNode = remDirNode nullImgDir removeChildRef

addChildRef :: ObjId -> ImgNode -> ImgNode
addChildRef r n = n & theDirEntries %~ S.insert r

-- | remove a child from an image dir node
removeChildRef :: ObjId -> ImgNode -> ImgNode
removeChildRef r n = n & theDirEntries %~ S.delete r

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
  toJSON (IP n t s c) = object $
    [ "Name"      .= n
    , "ImgType"   .= t
    , "TimeStamp" .= s
    ]
    ++
    if c == zeroCheckSum
    then []
    else ["CheckSum"  .= c]

instance FromJSON ImgPart where
  parseJSON = withObject "ImgPart" $ \ o ->
    IP <$> o .: "Name"
       <*> o .: "ImgType"
       <*> o .: "TimeStamp"
       <*> o .:? "CheckSum" .!= zeroCheckSum

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

type NameImgType = (Name, ImgType)
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

data ColEntry = ImgRef ObjId Name
              | ColRef ObjId

deriving instance Eq   ColEntry
deriving instance Ord  ColEntry
deriving instance Show ColEntry

instance ToJSON ColEntry where
  toJSON (ImgRef i n) = object
    [ "ColEntry"  .= ("IMG" :: String)
    , "ref"       .= i
    , "part"      .= n
    ]
  toJSON (ColRef i) = object
    [ "ColEntry"  .= ("COL" :: String)
    , "ref"       .= i
    ]

instance FromJSON ColEntry where
  parseJSON = withObject "ColEntry" $ \ o ->
    do t <- o .: "ColEntry"
       case t :: String of
         "IMG" ->
           ImgRef <$> o .: "ref"
                  <*> o .: "part"
         "COL" ->
           ColRef <$> o .: "ref"
         _ -> mzero

theColImgRef :: Prism' ColEntry (ObjId, Name)
theColImgRef =
  prism (uncurry ImgRef)
        (\ x -> case x of
            ImgRef i n -> Right (i, n)
            _          -> Left  x
        )

theColColRef :: Prism' ColEntry ObjId
theColColRef =
  prism ColRef
        (\ x -> case x of
            ColRef i -> Right i
            _        -> Left  x
        )

-- ----------------------------------------