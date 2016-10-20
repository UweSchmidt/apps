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
       , isColColRef
       , isColImgRef
       , colEntry
       , isoImgParts
       , isoImgPartsMap
       , isoDirEntries
       , theParts
       , thePartNames'
       , thePartNames
       , thePartNamesI
       , theImgName
       , theImgType
       , theImgTimeStamp
       , theImgCheckSum
       , theDir
       , theDirEntries
       , theSyncTime
       , theRootImgDir
       , theRootImgCol
       , theImgRoot
       , theImgCol
       , theColObjId
       , theColMetaData
       , theColImg
       , theColBlog
       , theColEntries
       , theColColRef
       , theColImgRef
       , addDirEntry
       , delDirEntry
       , delColEntry
       )
where

import           Control.Monad.Except
import           Data.MetaData
import           Data.Prim

import qualified Data.Aeson as J
import qualified Data.Map.Strict as M

-- ----------------------------------------


data ImgNode' ref = IMG  !ImgParts
                  | DIR  !(DirEntries' ref)    -- the contents of an image dir
                         !TimeStamp            -- the last sync with the file system
                  | ROOT !ref !ref
                  | COL  !MetaData             -- collection meta data
                         !(Maybe (ref, Name))  -- optional image
                         !(Maybe (ref, Name))  -- optional blog entry
                         ![ColEntry' ref]      -- the list of images and subcollections
                         !TimeStamp            -- last update

-- ----------------------------------------

deriving instance (Show ref) => Show (ImgNode' ref)

deriving instance Functor ImgNode'

instance IsEmpty (ImgNode' ref) where
  isempty (IMG _pts)               = True
  isempty (DIR es _ts)             = isempty es
  isempty (COL _md _im _be cs _ts) = isempty cs
  isempty (ROOT _d _c)             = False

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
    , t'archive     J..= rd
    , t'collections J..= rc
    ]
  toJSON (COL md im be es ts) = J.object $
    [ "ImgNode"    J..= ("COL" :: String)
    , "metadata"   J..= md
    , "entries"    J..= es
    , "sync"       J..= ts
    ]
    ++ case im of
         Nothing -> []
         Just p  -> ["image" J..= p]
    ++ case be of
         Nothing -> []
         Just p  -> ["blog"  J..= p]

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
           ROOT <$> o J..: t'archive
                <*> o J..: t'collections
         "COL" ->
           COL  <$> o J..: "metadata"
                <*> (Just <$> o J..:? "image") J..!= Nothing
                <*> (Just <$> o J..:? "blog" ) J..!= Nothing
                <*> o J..: "entries"
                <*> o J..: "sync"
         _ -> mzero

emptyImgDir :: ImgNode' ref
emptyImgDir = DIR mempty mempty
{-# INLINE emptyImgDir #-}

emptyImg :: ImgNode' ref
emptyImg = IMG mempty
{-# INLINE emptyImg #-}

emptyImgRoot :: Monoid ref => ImgNode' ref
emptyImgRoot = ROOT mempty mempty
{-# INLINE emptyImgRoot #-}

emptyImgCol :: ImgNode' ref
emptyImgCol = COL mempty Nothing Nothing [] mempty
{-# INLINE emptyImgCol #-}

-- image node optics

theParts :: Prism' (ImgNode' ref) ImgParts
theParts
  = prism IMG (\ x -> case x of
                  IMG m -> Right m
                  _     -> Left  x
              )
{-# INLINE theParts #-}

theDir :: Prism' (ImgNode' ref) (DirEntries' ref, TimeStamp)
theDir =
  prism (uncurry DIR)
        (\ x -> case x of
                DIR s t -> Right (s, t)
                _       -> Left  x
          )
{-# INLINE theDir #-}

theDirEntries :: Traversal' (ImgNode' ref) (DirEntries' ref)
theDirEntries = theDir . _1
{-# INLINE theDirEntries #-}

-- traverseWords :: Traverasl' State Word8
-- traverseWords :: Applicative f => (Word8 -> f Word8) -> State -> f State
-- traverseWords inj (State wa wb) = State <$> inj wa <*> inj wb

theSyncTime :: Traversal' (ImgNode' ref) TimeStamp
theSyncTime inj (DIR es ts)          = DIR es <$> inj ts
theSyncTime inj (COL md im be es ts) = COL md im be es <$> inj ts
theSyncTime _   n                    = pure n
{-# INLINE theSyncTime #-}

theImgRoot :: Prism' (ImgNode' ref) (ref, ref)
theImgRoot =
  prism (uncurry ROOT)
        (\ x -> case x of
            ROOT rd rc -> Right (rd, rc)
            _          -> Left x
        )
{-# INLINE theImgRoot #-}

theRootImgDir :: Traversal' (ImgNode' ref) ref
theRootImgDir = theImgRoot . _1
{-# INLINE theRootImgDir #-}

theRootImgCol :: Traversal' (ImgNode' ref) ref
theRootImgCol = theImgRoot . _2
{-# INLINE theRootImgCol #-}

theImgCol :: Prism' (ImgNode' ref)
                    (MetaData, Maybe (ref, Name), Maybe (ref, Name), [ColEntry' ref], TimeStamp)
theImgCol =
  prism (\ (x1, x2, x3, x4, x5) -> COL x1 x2 x3 x4 x5)
        (\ x -> case x of
            COL x1 x2 x3 x4 x5 -> Right (x1, x2, x3, x4, x5)
            _                  -> Left x
        )
{-# INLINE theImgCol #-}

theColMetaData :: Traversal' (ImgNode' ref) MetaData
theColMetaData = theImgCol . _1
{-# INLINE theColMetaData #-}

theColImg :: Traversal' (ImgNode' ref) (Maybe (ref, Name))
theColImg = theImgCol . _2
{-# INLINE theColImg #-}

theColBlog :: Traversal' (ImgNode' ref) (Maybe (ref, Name))
theColBlog = theImgCol . _3
{-# INLINE theColBlog #-}

theColEntries :: Traversal' (ImgNode' ref) [ColEntry' ref]
theColEntries = theImgCol . _4
{-# INLINE theColEntries #-}

isDIR :: ImgNode' ref -> Bool
isDIR DIR{}  = True
isDIR _      = False
{-# INLINE isDIR #-}

isIMG :: ImgNode' ref -> Bool
isIMG IMG{}  = True
isIMG _      = False
{-# INLINE isIMG #-}

isROOT :: ImgNode' ref -> Bool
isROOT ROOT{} = True
isROOT _      = False
{-# INLINE isROOT #-}

isCOL :: ImgNode' ref -> Bool
isCOL COL{} = True
isCOL _     = False
{-# INLINE isCOL #-}

-- ----------------------------------------

newtype ImgParts = ImgParts (Map Name ImgPart)

deriving instance Show ImgParts

instance IsEmpty ImgParts where
  isempty (ImgParts im) = isempty im
  {-# INLINE isempty #-}

instance Monoid ImgParts where
  mempty = ImgParts M.empty
  {-# INLINE mempty #-}

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
  {-# INLINE toJSON #-}

instance FromJSON ImgParts where
  parseJSON x = (ImgParts . M.fromList) <$> parseJSON x

mkImgParts :: [ImgPart] -> ImgParts
mkImgParts ps = ps ^. from isoImgParts
{-# INLINE mkImgParts #-}

isoImgParts :: Iso' ImgParts [ImgPart]
isoImgParts =
  iso (\ (ImgParts pm) -> pm) ImgParts
  .
  isoMapElems (\ (IP n _ _ _) -> n)
{-# INLINE isoImgParts #-}

isoImgPartsMap :: Iso' ImgParts (Map Name ImgPart)
isoImgPartsMap = iso (\ (ImgParts pm) -> pm) ImgParts

thePartNames' :: (ImgType -> Bool) -> Traversal' ImgParts Name
thePartNames' typTest =
  isoImgParts . traverse . isA (^. theImgType . to typTest) . theImgName
{-# INLINE thePartNames' #-}

-- images with 1 of the given types can be rendered
thePartNamesI :: Traversal' ImgParts Name
thePartNamesI = thePartNames' (`elem` [IMGjpg, IMGimg, IMGtxt])
{-# INLINE thePartNamesI #-}

thePartNames :: Traversal' ImgParts Name
thePartNames = thePartNames' (const True)
{-# INLINE thePartNames #-}

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
{-# INLINE mkImgPart #-}

theImgName :: Lens' ImgPart Name
theImgName k (IP n t s c) = (\ new -> IP new t s c) <$> k n
{-# INLINE theImgName #-}

theImgType :: Lens' ImgPart ImgType
theImgType k (IP n t s c) = (\ new -> IP n new s c) <$> k t
{-# INLINE theImgType #-}

theImgTimeStamp :: Lens' ImgPart TimeStamp
theImgTimeStamp k (IP n t s c) = (\ new -> IP n t new c) <$> k s
{-# INLINE theImgTimeStamp #-}

theImgCheckSum :: Lens' ImgPart CheckSum
theImgCheckSum k (IP n t s c) = (\ new -> IP n t s new) <$> k c
{-# INLINE theImgCheckSum #-}

-- ----------------------------------------

data ColEntry' ref = ImgRef !ref !Name !MetaData
                   | ColRef !ref

deriving instance (Show ref) => Show (ColEntry' ref)

deriving instance Functor ColEntry'

instance (ToJSON ref) => ToJSON (ColEntry' ref) where
  toJSON (ImgRef i n m) = J.object $
    [ "ColEntry"  J..= ("IMG" :: String)
    , "ref"       J..= i
    , "part"      J..= n
    ]
    ++ ("metadata" .=?! m)        -- optional meta data (title, comment, ...)

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
                  <*> o   .:?! "metadata"
         "COL" ->
           ColRef <$> o J..: "ref"
         _ -> mzero

mkColImgRef :: ref -> Name -> (ColEntry' ref)
mkColImgRef i n = ImgRef i n mempty
{-# INLINE mkColImgRef #-}

mkColColRef :: ref -> (ColEntry' ref)
mkColColRef = ColRef
{-# INLINE mkColColRef #-}

colEntry :: (ref -> Name -> MetaData -> a) ->
            (ref -> a) ->
            ColEntry' ref -> a
colEntry  imgRef _colRef (ImgRef i n md) = imgRef i n md
colEntry _imgRef  colRef (ColRef i     ) = colRef i


theColObjId :: Lens' (ColEntry' ref) ref
theColObjId k (ImgRef i n m) = (\ new -> ImgRef new n m) <$> k i
theColObjId k (ColRef i)     = (\ new -> ColRef new)     <$> k i
{-# INLINE theColObjId #-}

-- theImgName :: Lens' ImgPart Name
-- theImgName k (IP n t s c) = (\ new -> IP new t s c) <$> k n

theColImgRef :: Prism' (ColEntry' ref) (ref, Name, MetaData)
theColImgRef =
  prism (\ (i, n, m) -> ImgRef i n m)
        (\ x -> case x of
            ImgRef i n m -> Right (i, n, m)
            _            -> Left  x
        )
{-# INLINE theColImgRef #-}

theColColRef :: Prism' (ColEntry' ref) ref
theColColRef =
  prism ColRef
        (\ x -> case x of
            ColRef i -> Right i
            _        -> Left  x
        )
{-# INLINE theColColRef #-}

isColColRef
  , isColImgRef :: ColEntry' ref -> Bool

isColColRef (ColRef{}) = True
isColColRef _          = False

isColImgRef = not . isColColRef

-- ----------------------------------------

newtype DirEntries' ref = DE [ref]

deriving instance (Eq   ref) => Eq   (DirEntries' ref)
deriving instance (Ord  ref) => Ord  (DirEntries' ref)
deriving instance (Show ref) => Show (DirEntries' ref)

deriving instance Functor DirEntries'

instance IsEmpty (DirEntries' ref) where
  isempty (DE xs) = isempty xs
  {-# INLINE isempty #-}

instance Monoid (DirEntries' ref) where
  mempty = DE []
  {-# INLINE mempty #-}

  DE xs `mappend` DE ys = DE $ xs ++ ys
  {-# INLINE mappend #-}

instance (ToJSON ref) => ToJSON (DirEntries' ref) where
  toJSON (DE rs) = toJSON rs
  {-# INLINE toJSON #-}

instance (FromJSON ref) => FromJSON (DirEntries' ref) where
  parseJSON rs = DE <$> parseJSON rs
  {-# INLINE parseJSON #-}

mkDirEntries :: [ref] -> DirEntries' ref
mkDirEntries = DE
{-# INLINE mkDirEntries #-}

isoDirEntries :: Iso' (DirEntries' ref) [ref]
isoDirEntries = iso (\ (DE xs) -> xs) DE
{-# INLINE isoDirEntries #-}

addDirEntry :: ref -> DirEntries' ref -> DirEntries' ref
addDirEntry r (DE rs) = DE $ r : rs
{-# INLINE addDirEntry #-}

delDirEntry :: (Eq ref) => ref -> DirEntries' ref -> DirEntries' ref
delDirEntry r (DE rs) = DE $ filter (/= r) rs
{-# INLINE delDirEntry #-}

delColEntry :: (Eq ref) => ref -> [ColEntry' ref] -> [ColEntry' ref]
delColEntry r cs = filter (\ ce -> ce ^. theColObjId /= r) cs
{-# INLINE delColEntry #-}


-- ----------------------------------------
