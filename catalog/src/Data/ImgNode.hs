{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.ImgNode
       ( ImgNode'(..)
       , ImgParts
       , ImgPart
       , ColEntry'(..)
       , DirEntries'
       , mkImgParts
       , mkImgPart
       , mkColImgRef
       , mkColColRef
       , mkDirEntries
       , emptyImg
       , emptyImgDir
       , emptyImgRoot
       , emptyImgCol
       , isDIR
       , isIMG
       , isROOT
       , isCOL
       , isoImgParts
       , isoDirEntries
       , theParts
       , thePartNames
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
       , theColImgObjId
       , theColMetaData
       , theColEntries
       , theColSyncTime
       , theColColRef
       , theColImgRef
       , addDirEntry
       , delDirEntry
       )
where

import           Control.Monad.Except
import           Data.MetaData
import           Data.Prim

import qualified Data.Aeson as J
import qualified Data.Map.Strict as M

-- ----------------------------------------


data ImgNode' ref = IMG  !ImgParts
                  | DIR  !(DirEntries' ref) !TimeStamp
                  | ROOT !ref !ref
                  | COL  !MetaData ![ColEntry' ref] !TimeStamp

-- ----------------------------------------

deriving instance (Show ref) => Show (ImgNode' ref)

deriving instance Functor ImgNode'

instance IsEmpty (ImgNode' ref) where
  isempty (IMG pts)        = isempty pts
  isempty (DIR es _ts)     = isempty es
  isempty (COL _md cs _ts) = isempty cs
  isempty (ROOT _d _c)     = False

instance ToJSON ref => ToJSON (ImgNode' ref) where
  toJSON (IMG pm) = J.object
    [ "ImgNode"     J..= ("IMG" :: String)
    , "parts"       J..= pm
    ]
  toJSON (DIR rs ts) = J.object
    [ "ImgNode"     J..= ("DIR" :: String)
    , "children"    J..= rs
    , "sync"        J..= ts
    ]
  toJSON (ROOT rd rc) = J.object
    [ "ImgNode"     J..= ("ROOT" :: String)
    , "archive"     J..= rd
    , "collections" J..= rc
    ]
  toJSON (COL md es ts) = J.object
    [ "ImgNode"    J..= ("COL" :: String)
    , "metadata"   J..= md
    , "entries"    J..= es
    , "sync"       J..= ts
    ]

instance (Ord ref, FromJSON ref) => FromJSON (ImgNode' ref) where
  parseJSON = J.withObject "ImgNode" $ \ o ->
    do t <- o J..: "ImgNode"
       case t :: String of
         "IMG" ->
           IMG  <$> o J..: "parts"
         "DIR" ->
           DIR  <$> o J..: "children"
                <*> o J..:? "sync" J..!= mempty
         "ROOT" ->
           ROOT <$> o J..: "archive"
                <*> o J..: "collections"
         "COL" ->
           COL  <$> o J..: "metadata"
                <*> o J..: "entries"
                <*> o J..: "sync"
         _ -> mzero

emptyImgDir :: ImgNode' ref
emptyImgDir = DIR mempty mempty

emptyImg :: ImgNode' ref
emptyImg = IMG mempty

emptyImgRoot :: Monoid ref => ImgNode' ref
emptyImgRoot = ROOT mempty mempty

emptyImgCol :: ImgNode' ref
emptyImgCol = COL mempty [] mempty

-- image node optics

theParts :: Prism' (ImgNode' ref) ImgParts
theParts
  = prism IMG (\ x -> case x of
                  IMG m -> Right m
                  _     -> Left  x
              )

theDir :: Prism' (ImgNode' ref) (DirEntries' ref, TimeStamp)
theDir =
  prism (uncurry DIR)
        (\ x -> case x of
                DIR s t -> Right (s, t)
                _       -> Left  x
          )

theDirEntries :: Traversal' (ImgNode' ref) (DirEntries' ref)
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

theImgCol :: Prism' (ImgNode' ref) (MetaData, [ColEntry' ref], TimeStamp)
theImgCol
  = prism (\ (x1, x2, x3) -> COL x1 x2 x3)
          (\ x -> case x of
              COL x1 x2 x3 -> Right (x1, x2, x3)
              _            -> Left x
          )

theColMetaData :: Traversal' (ImgNode' ref) MetaData
theColMetaData = theImgCol . _1

theColEntries :: Traversal' (ImgNode' ref) [ColEntry' ref]
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

-- ----------------------------------------

newtype ImgParts = ImgParts (Map Name ImgPart)

deriving instance Show ImgParts

instance IsEmpty ImgParts where
  isempty (ImgParts im) = isempty im

instance Monoid ImgParts where
  mempty = ImgParts M.empty

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

mkImgParts :: [ImgPart] -> ImgParts
mkImgParts ps = ps ^. from isoImgParts

isoImgParts :: Iso' ImgParts [ImgPart]
isoImgParts =
  iso (\ (ImgParts pm) -> pm) ImgParts
  .
  isoMapElems (\ (IP n _ _ _) -> n)

thePartNames :: ImgType -> Traversal' ImgParts Name
thePartNames ty = isoImgParts . traverse . isA (^. theImgType . to (== ty)) . theImgName

-- ----------------------------------------

data ImgPart     = IP !Name !ImgType !TimeStamp !CheckSum

deriving instance Show ImgPart

instance ToJSON ImgPart where
  toJSON (IP n t s c) = J.object $
    [ "Name"      J..= n
    , "ImgType"   J..= t
    ]
    ++ ("TimeStamp" .=?! s)       -- optional field
    ++ ("CheckSum"  .=?! c)       --     "      "

instance FromJSON ImgPart where
  parseJSON = J.withObject "ImgPart" $ \ o ->
    IP <$> o J..:   "Name"
       <*> o J..:   "ImgType"
       <*> o   .:?! "TimeStamp"   -- optional field
       <*> o   .:?! "CheckSum"    --    "       "

mkImgPart :: Name -> ImgType -> ImgPart
mkImgPart n t = IP n t mempty mempty

theImgName :: Lens' ImgPart Name
theImgName k (IP n t s c) = (\ new -> IP new t s c) <$> k n

theImgType :: Lens' ImgPart ImgType
theImgType k (IP n t s c) = (\ new -> IP n new s c) <$> k t

theImgTimeStamp :: Lens' ImgPart TimeStamp
theImgTimeStamp k (IP n t s c) = (\ new -> IP n t new c) <$> k s

theImgCheckSum :: Lens' ImgPart CheckSum
theImgCheckSum k (IP n t s c) = (\ new -> IP n t s new) <$> k c

-- ----------------------------------------

data ColEntry' ref = ImgRef ref Name
                   | ColRef ref

deriving instance (Eq   ref) => Eq   (ColEntry' ref)
deriving instance (Ord  ref) => Ord  (ColEntry' ref)
deriving instance (Show ref) => Show (ColEntry' ref)

deriving instance Functor ColEntry'

instance (ToJSON ref) => ToJSON (ColEntry' ref) where
  toJSON (ImgRef i n) = J.object
    [ "ColEntry"  J..= ("IMG" :: String)
    , "ref"       J..= i
    , "part"      J..= n
    ]
  toJSON (ColRef i) = J.object
    [ "ColEntry"  J..= ("COL" :: String)
    , "ref"       J..= i
    ]

instance (FromJSON ref) => FromJSON (ColEntry' ref) where
  parseJSON = J.withObject "ColEntry" $ \ o ->
    do t <- o J..: "ColEntry"
       case t :: String of
         "IMG" ->
           ImgRef <$> o J..: "ref"
                  <*> o J..: "part"
         "COL" ->
           ColRef <$> o J..: "ref"
         _ -> mzero

mkColImgRef :: ref -> Name -> (ColEntry' ref)
mkColImgRef = ImgRef

mkColColRef :: ref -> (ColEntry' ref)
mkColColRef = ColRef

theColImgObjId :: Lens' (ColEntry' ref) ref
theColImgObjId k (ImgRef i n) = (\ new -> ImgRef new n) <$> k i
theColImgObjId k (ColRef i)   = (\ new -> ColRef new)   <$> k i


-- theImgName :: Lens' ImgPart Name
-- theImgName k (IP n t s c) = (\ new -> IP new t s c) <$> k n


theColImgRef :: Prism' (ColEntry' ref) (ref, Name)
theColImgRef =
  prism (uncurry ImgRef)
        (\ x -> case x of
            ImgRef i n -> Right (i, n)
            _          -> Left  x
        )

theColColRef :: Prism' (ColEntry' ref) ref
theColColRef =
  prism ColRef
        (\ x -> case x of
            ColRef i -> Right i
            _        -> Left  x
        )

-- ----------------------------------------

newtype DirEntries' ref = DE [ref]

deriving instance (Eq   ref) => Eq   (DirEntries' ref)
deriving instance (Ord  ref) => Ord  (DirEntries' ref)
deriving instance (Show ref) => Show (DirEntries' ref)

deriving instance Functor DirEntries'

instance IsEmpty (DirEntries' ref) where
  isempty (DE xs) = isempty xs

instance Monoid (DirEntries' ref) where
  mempty = DE []
  DE xs `mappend` DE ys = DE $ xs ++ ys

instance (ToJSON ref) => ToJSON (DirEntries' ref) where
  toJSON (DE rs) = toJSON rs

instance (FromJSON ref) => FromJSON (DirEntries' ref) where
  parseJSON rs = DE <$> parseJSON rs

mkDirEntries :: [ref] -> DirEntries' ref
mkDirEntries = DE

isoDirEntries :: Iso' (DirEntries' ref) [ref]
isoDirEntries = iso (\ (DE xs) -> xs) DE

addDirEntry :: ref -> DirEntries' ref -> DirEntries' ref
addDirEntry r (DE rs) = DE $ r : rs

delDirEntry :: (Eq ref) => ref -> DirEntries' ref -> DirEntries' ref
delDirEntry r (DE rs) = DE $ filter (/= r) rs


-- ----------------------------------------
