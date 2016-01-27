{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Prelude
       ( module Catalog.Prelude
       , module Control.Monad.RWSErrorIO
       , module Text.Regex.XMLSchema.Generic
       )
where

import           Data.Prim.CheckSum
import           Data.Prim.Name
import           Data.Prim.ObjId
import           Data.Prim.Path
import           Data.Prim.TimeStamp

import           Control.Lens hiding (children)
import           Control.Arrow (first, (***))
import           Control.Applicative
import           Control.Monad.RWSErrorIO
import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)

import           Data.Map.Strict (Map)
-- import           Data.Monoid
import           Data.List (intercalate, partition)

import           Data.Aeson hiding (Object, (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J


import           Data.Maybe
import           System.FilePath ((</>))
import qualified System.Posix as X
import           System.Posix (FileStatus)
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map.Strict as M

-- import qualified Data.Text as T

-- ----------------------------------------

data ObjStore     = ObjStore
                    { _objMap    :: ! ObjMap
                    , _root      :: ! ObjId
                    , _mountPath :: ! Name
                    }

newtype ObjMap    = ObjMap (M.Map ObjId Object)

data Object       = Object
                    { _otype :: ! ObjType
                    , _orel  :: ! ObjRels
                    , _fse   :: ! FSEntry
                    }

data ObjType      = DIR | RAW | JPG | COPY | XMP | EXIF

newtype ObjRels   = ObjRels (Map RelType RelVal)

data RelType      = CHILDREN | PARENT | VERSIONS | ORIG | COPIES

data RelVal       = RelSingle    ! ObjId
                  | RelOrdered   ! [(Name, ObjId)]
                  | RelUnordered ! (Map Name ObjId)
                  | RelCopies    ! (Map ImgSize ObjId)

data FSEntry      = FSEntry
                    { _filePath :: ! Name          -- ^ the corresponding fs entry stored in compact form
                    , _age      :: ! TimeStamp     -- ^ the timestamp for last write of the fs entry
                    , _checksum :: ! CheckSum      -- ^ the checksum of the file contents
                    }

data    ImgSize   = IS ! Int ! Int

-- ----------------------------------------
--
-- basic ops for the object store

-- makeLenses ''ObjStore

deriving instance Show ObjStore

emptyObjStore :: ObjStore
emptyObjStore =
  ObjStore
  { _objMap    = emptyObjMap
  , _root      = emptyObjId
  , _mountPath = emptyName
  }

-- lenses

objMap :: Lens' ObjStore ObjMap
objMap k s = fmap (\ new -> s { _objMap = new }) (k (_objMap s) )

root :: Lens' ObjStore (Maybe ObjId)
root = root' . objId2Maybe
  where
    root' :: Lens' ObjStore ObjId
    root' k s = fmap (\ new -> s { _root = new }) (k (_root s) )

mountPath :: Lens' ObjStore String
mountPath = mountPath' . name2string
  where
    mountPath' :: Lens' ObjStore Name
    mountPath' k s = fmap (\ new -> s { _mountPath = new }) (k (_mountPath s) )

instance ToJSON ObjStore where
  toJSON o = J.object
    [ "objMap"    J..= _objMap    o
    , "root"      J..= _root      o
    , "mountPath" J..= _mountPath o
    ]

instance FromJSON ObjStore where
  parseJSON = withObject "ObjStore" $ \ o ->
    ObjStore
    <$> o .: "objMap"
    <*> o .: "root"
    <*> o .: "mountPath"

-- ----------------------------------------
--
-- basic ops for the object map

emptyObjMap :: ObjMap
emptyObjMap = ObjMap M.empty

deriving instance Show ObjMap

isoObjMap :: Iso' ObjMap (M.Map ObjId Object)
isoObjMap = iso (\ (ObjMap om) -> om) ObjMap

instance ToJSON ObjMap where
  toJSON (ObjMap om) = toJSON $ M.toList om

instance FromJSON ObjMap where
  parseJSON j = (ObjMap . M.fromList) <$> parseJSON j

lookupObjMap :: ObjId -> ObjMap -> Maybe Object
lookupObjMap oid (ObjMap om) = M.lookup oid om

insertObjMap :: ObjId -> Object -> ObjMap -> ObjMap
insertObjMap oid obj (ObjMap om) = ObjMap $ M.insert oid obj om

deleteObjMap :: ObjId -> ObjMap -> ObjMap
deleteObjMap oid (ObjMap om) = ObjMap $ M.delete oid om

memberObjM :: ObjId -> Cmd Bool
memberObjM oid = uses (objMap . isoObjMap) (maybe False (const True) . M.lookup oid)

lookupObjM :: ObjId -> Cmd Object
lookupObjM oid = do
  res <- uses (objMap . isoObjMap) (M.lookup oid)
  case res of
    Nothing -> abort $ "lookupObjM: object not found " ++ show oid
    Just o  -> return o

checkObjM :: (Object -> Bool) -> ObjId -> Cmd Object
checkObjM p oid = do
  o <- lookupObjM oid
  if p o
    then return o
    else abort $ "checkObjM: wrong object type for object " ++ show o

insertObjM :: ObjId -> Object -> Cmd ()
insertObjM oid obj = objMap . isoObjMap %= M.insert oid obj

deleteObjM :: ObjId -> Cmd ()
deleteObjM oid = objMap . isoObjMap %= M.delete oid

adjustObjM :: (Object -> Object) -> ObjId -> Cmd ()
adjustObjM f oid = objMap . isoObjMap %= M.adjust f oid

-- ----------------------------------------
--
-- basic ops for object

-- makeLenses ''Object

mkObject :: ObjType -> Object
mkObject t = Object t emptyObjRels emptyFSEntry

mkObjectFS :: ObjType -> FSEntry -> Object
mkObjectFS t fs = Object t emptyObjRels fs

otype :: Lens' Object ObjType
otype k o = fmap (\ new -> o { _otype = new }) (k (_otype o) )

orels :: Lens' Object ObjRels
orels k o = fmap (\ new -> o { _orel = new }) (k (_orel o) )

fse :: Lens' Object FSEntry
fse k o = fmap (\ new -> o { _fse = new }) (k (_fse o) )

deriving instance Show Object

instance ToJSON Object where
  toJSON o = J.object $
    ( let f = o ^. fse in
      if nullFSEntry f
      then []
      else [ "fse" J..= f]
    )
    ++
    [ "otype" J..= _otype o
    , "orel"  J..= _orel  o
    ]

instance FromJSON Object where
  parseJSON = withObject "Object" $ \ o ->
    Object
    <$> o .: "otype"
    <*> o .: "orel"
    <*> ( (o .: "fse")
          <|>
          return emptyFSEntry
        )

-- ----------------------------------------
--
-- basic ops for object types

deriving instance Eq   ObjType
deriving instance Ord  ObjType
deriving instance Read ObjType
deriving instance Show ObjType

instance ToJSON ObjType where
  toJSON = toJSON . show

instance FromJSON ObjType where
  parseJSON o = read <$> parseJSON o

-- ----------------------------------------
--
-- tests for object types

isDirObj :: Object -> Bool
isDirObj = hasObjType DIR

isRawObj :: Object -> Bool
isRawObj = hasObjType RAW

isJpgObj :: Object -> Bool
isJpgObj = hasObjType JPG

isCopyObj :: Object -> Bool
isCopyObj = hasObjType COPY

isXmpObj :: Object -> Bool
isXmpObj = hasObjType XMP

isExifObj :: Object -> Bool
isExifObj = hasObjType EXIF

hasObjType :: ObjType -> Object -> Bool
hasObjType ot (Object {_otype = ot2}) = ot == ot2

-- ----------------------------------------
--
-- lenses for Object to access relations

children :: Lens' Object RelVal
children = orels . relAt CHILDREN

childrenIds :: Lens' Object (Map Name ObjId)
childrenIds = children . relUnordered

parent :: Lens' Object RelVal
parent = orels . relAt PARENT

parentId :: Lens' Object (Maybe ObjId)
parentId = parent . relSingle . objId2Maybe

versions :: Lens' Object RelVal
versions = orels . relAt VERSIONS

versionsIds :: Lens' Object (Map Name ObjId)
versionsIds = versions . relUnordered

orig :: Lens' Object RelVal
orig = orels . relAt ORIG

origId :: Lens' Object (Maybe ObjId)
origId = orig . relSingle .objId2Maybe

copies :: Lens' Object RelVal
copies = orels . relAt COPIES

copiesIds :: Lens' Object (Map ImgSize ObjId)
copiesIds = copies . relCopies

-- ----------------------------------------
--
-- relation setters

addChild :: Name -> ObjId -> Object -> Object
addChild n oid = childrenIds . at n .~ Just oid

remChild :: Name -> Object -> Object
remChild n = childrenIds . at n .~ Nothing

setParent :: ObjId -> Object -> Object
setParent oid = parentId .~ Just oid

addVersion :: Name -> ObjId -> Object -> Object
addVersion n oid = versionsIds . at n .~ Just oid

remVersion :: Name -> Object -> Object
remVersion n = versionsIds . at n .~ Nothing

setOrig :: ObjId -> Object -> Object
setOrig oid = origId .~ Just oid

addCopy :: ImgSize -> ObjId -> Object -> Object
addCopy n oid = copiesIds . at n .~ Just oid

remCopy :: ImgSize -> Object -> Object
remCopy n = copiesIds . at n .~ Nothing

-- ----------------------------------------
--
-- basic ops for object relations

emptyObjRels :: ObjRels
emptyObjRels = ObjRels M.empty

singletonObjRels :: RelType -> RelVal -> ObjRels
singletonObjRels rt rv = ObjRels $ M.singleton rt rv

deriving instance Eq   ObjRels
deriving instance Show ObjRels

isoObjRels :: Iso' ObjRels (Map RelType RelVal)
isoObjRels = iso (\ (ObjRels r) -> r) ObjRels

instance ToJSON ObjRels where
  toJSON (ObjRels rm) = toJSON $ M.toList rm

instance FromJSON ObjRels where
  parseJSON j = (ObjRels . M.fromList) <$> parseJSON j

instance Monoid ObjRels where
  mempty  = emptyObjRels
  mappend = mergeObjRels zipConst2

mergeObjRels :: ZipRelVal -> ObjRels -> ObjRels -> ObjRels
mergeObjRels ops (ObjRels or1) (ObjRels or2) =
  ObjRels $ M.mergeWithKey combine id id or1 or2
  where
    combine _rt = mergeRelVal ops

relAt :: RelType -> Lens' ObjRels RelVal
relAt rt = \ k rel -> fmap (\ new -> ins rt new rel) (k (look rt rel))
  where
    ins t new rel = rel <> singletonObjRels t new

    look t (ObjRels rm) = maybe (def t) id $ M.lookup t rm
      where
        def CHILDREN = RelUnordered M.empty
        def PARENT   = RelSingle      emptyObjId
        def VERSIONS = RelUnordered M.empty
        def ORIG     = RelSingle      emptyObjId
        def COPIES   = RelCopies    M.empty

-- ----------------------------------------
--
-- basic ops for relations types

deriving instance Eq   RelType
deriving instance Ord  RelType
deriving instance Read RelType
deriving instance Show RelType

instance ToJSON RelType where
  toJSON = toJSON . show

instance FromJSON RelType where
  parseJSON o = read <$> parseJSON o

-- ----------------------------------------
--
-- basic ops for object relation values

emptyRelVal :: RelVal
emptyRelVal = RelSingle emptyObjId

deriving instance Eq   RelVal
deriving instance Ord  RelVal
deriving instance Show RelVal

relSingle :: Lens' RelVal ObjId
relSingle k r = fmap RelSingle (k (sel r))
  where
    sel (RelSingle o) = o
    sel _             = emptyObjId

relOrdered :: Lens' RelVal [(Name, ObjId)]
relOrdered k r = fmap RelOrdered (k (sel r))
  where
    sel (RelOrdered o) = o
    sel _              = []

relUnordered :: Lens' RelVal (Map Name ObjId)
relUnordered k r = fmap RelUnordered (k (sel r))
  where
    sel (RelUnordered o) = o
    sel _                = M.empty

relCopies :: Lens' RelVal (Map ImgSize ObjId)
relCopies k r = fmap RelCopies (k (sel r))
  where
    sel (RelCopies o) = o
    sel _             = M.empty

instance ToJSON RelVal where
  toJSON (RelSingle oid) = J.object
    [ "single" J..= oid]

  toJSON (RelOrdered ns) = J.object
    [ "ordered" J..= ns]

  toJSON (RelUnordered nm) = J.object
    [ "unordered" J..= M.toList nm]

  toJSON (RelCopies cm) = J.object
    [ "copies" J..= map (first showImgSize) (M.toList cm) ]

instance FromJSON RelVal where
  parseJSON = withObject "RelVal" $ \ o ->
    ( do oid <- o .: "single"
         RelSingle <$> parseJSON oid
    ) <|>
    ( do ns <- o .: "ordered"
         RelOrdered <$> parseJSON ns
    ) <|>
    ( do nm <- o .: "unordered"
         (RelUnordered . M.fromList) <$> parseJSON nm
    ) <|>
    ( do cm <- o .: "copies"
         (RelCopies . M.fromList . map (first readImgSize)) <$> parseJSON cm
    )

-- ----------------------------------------
--
-- basic ops for file system entries

emptyFSEntry :: FSEntry
emptyFSEntry = FSEntry
  { _filePath = emptyName
  , _age      = zeroTimeStamp
  , _checksum = zeroCheckSum
  }

nullFSEntry :: FSEntry -> Bool
nullFSEntry = nullName . _filePath

mkFSEntry :: FilePath -> FSEntry
mkFSEntry p = emptyFSEntry { _filePath = mkName p}

filePath :: Lens' FSEntry FilePath
filePath = filePath' . name2string
  where
    filePath' :: Lens' FSEntry Name
    filePath' k f = fmap (\ new -> f { _filePath = new }) (k (_filePath f) )

age :: Lens' FSEntry TimeStamp
age k f = fmap (\ new -> f { _age = new }) (k (_age f) )

checksum :: Lens' FSEntry CheckSum
checksum k f = fmap (\ new -> f { _checksum = new }) (k (_checksum f) )

deriving instance Show FSEntry

instance ToJSON FSEntry where
  toJSON (FSEntry { _filePath = p
                  , _age      = t
                  , _checksum = cs
                  }
         )
    | nullName p = J.object []
    | otherwise  = J.object
      [ "filePath" J..= toJSON p
      , "age"      J..= toJSON t
      , "checksum" J..= toJSON cs
      ]

instance FromJSON FSEntry where
  parseJSON = withObject "FSEntry" $ \ o ->
    ( FSEntry
      <$> o .: "filePath"
      <*> o .: "age"
      <*> o .: "checksum"
    )
    <|> return emptyFSEntry

-- ----------------------------------------
--
-- basic ops for image sizes

mkImgSize :: Int -> Int -> ImgSize
mkImgSize = IS

fromImgSize :: ImgSize -> (Int, Int)
fromImgSize (IS w h) = (w, h)

deriving instance Eq  ImgSize
deriving instance Ord ImgSize

showImgSize :: ImgSize -> String
showImgSize (IS w h) = show w ++ "x" ++ show h

readImgSize :: String -> ImgSize
readImgSize s
  = IS (read w) (read h)
  where
    (w, _ : h) = span (/= 'x') s

instance Show ImgSize where
  show = showImgSize

instance ToJSON ImgSize where
  toJSON (IS w h) = J.object
    [ "width"  J..= w
    , "height" J..= h
    ]

instance FromJSON ImgSize where
  parseJSON = withObject "ImgSize" $ \o ->
    IS
    <$> o .: "width"
    <*> o .: "height"

-- ----------------------------------------
--
-- auxiliary data types and ops for merging relations

data ZipRelVal =
  ZipRelVal
  { zipSingle    :: ObjId             -> ObjId             -> Maybe ObjId
  , zipOrdered   :: [(Name, ObjId)]   -> [(Name, ObjId)]   -> Maybe [(Name, ObjId)]
  , zipUnordered :: Map Name ObjId    -> Map Name ObjId    -> Maybe (Map Name ObjId)
  , zipCopies    :: Map ImgSize ObjId -> Map ImgSize ObjId -> Maybe (Map ImgSize ObjId)
  }


zipConst2 :: ZipRelVal
zipConst2 = ZipRelVal
  { zipSingle    = \ _x y -> Just y
  , zipOrdered   = \ _x y -> Just y
  , zipUnordered = \ _x y -> Just y
  , zipCopies    = \ _x y -> Just y
  }

zipConst1 :: ZipRelVal
zipConst1 = ZipRelVal
  { zipSingle    = \ x _y -> Just x
  , zipOrdered   = \ x _y -> Just x
  , zipUnordered = \ x _y -> Just x
  , zipCopies    = \ x _y -> Just x
  }

mergeRelVal :: ZipRelVal -> RelVal -> RelVal -> Maybe RelVal
mergeRelVal ops x1 x2 = merge x1 x2 >>= cleanup
  where
    merge (RelSingle    r1) (RelSingle    r2) = RelSingle    <$> zipSingle    ops r1 r2
    merge (RelOrdered   r1) (RelOrdered   r2) = RelOrdered   <$> zipOrdered   ops r1 r2
    merge (RelUnordered r1) (RelUnordered r2) = RelUnordered <$> zipUnordered ops r1 r2
    merge (RelCopies    r1) (RelCopies    r2) = RelCopies    <$> zipCopies    ops r1 r2
    merge _                 _                 = Nothing

    cleanup (RelSingle    r1) | nullObjId r1 = Nothing
    cleanup (RelOrdered   r1) | null r1      = Nothing
    cleanup (RelUnordered r1) | M.null r1    = Nothing
    cleanup (RelCopies    r1) | M.null r1    = Nothing
    cleanup r                                = Just r

-- ----------------------------------------
--
-- other stuff

filterObj :: (Object -> Bool) -> ObjId -> ObjMap -> Maybe Object
filterObj p oid om = do
  o <- lookupObjMap oid om
  if p o
    then return o
    else mzero

removeExObj :: ObjId -> ObjMap -> Maybe ObjMap
removeExObj oid (ObjMap om)
  | oid `M.member` om =
      Just $ ObjMap $ M.delete oid om
  | otherwise =
      Nothing

-- ----------------------------------------
--
-- mothers little helpers

infixr 2 |||

-- | Lift boolean 'or' over predicates.
(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p ||| q = \ v -> p v || q v

groupBy :: (Ord e) => (a -> e) -> [a] -> [[a]]
groupBy f =
  M.elems
  . foldr (\ x m -> M.insertWith (++) (f x) [x]
                    m) M.empty

-- ----------------------------------------

data Env = Env

instance Config Env where

type Cmd = Action Env ObjStore

runCmd :: Cmd a -> IO (Either Msg a, ObjStore, Log)
runCmd cmd = runAction cmd Env emptyObjStore

syncFS :: Cmd ()
syncFS = do
  notMounted <- uses root isNothing -- asks _root
  when notMounted
       mountFS

  rootObjId <- uses root fromJust
  syncFSEntry rootObjId
  return ()

syncFSEntry :: ObjId -> Cmd ()
syncFSEntry oid = do
  o <- lookupObjM oid
  trc $ "syncFSentry: syncing oid " ++ show oid ++ " => " ++ show o
  p <- absPath o
  trc $ "syncFSentry: absPath " ++ show p

  x <- io $ X.fileExist p
  if x
    then do
      status <- io $ X.getFileStatus p
      let newtime = TS $ X.modificationTime status
      let oldtime = o ^. fse .age
      let update  = newtime > oldtime
      when update $ adjustObjM (fse . age .~ newtime) oid

      case o ^. otype of
        DIR -> do
          trc $ "syncFSentry: update directory"
          when update $ do
            trc $ "syncFSentry: read directory entries"
            es <- scanFSDir p
            trc $ "syncFSentry: dir contents " ++ show es
            syncDir oid es

          syncDirEntries oid

        _oty -> do
          trc $ "syncFSentry: update file"
          when update $ do
            trc $ "syncFSentry: file has changed"

    else do
      warn $ "syncFSentry: file object not found, will be removed " ++ show p
      deleteObjects oid

deleteObjects :: ObjId -> Cmd ()
deleteObjects oid = do
  o <- lookupObjM oid
  p <- objPath o
  warn $ "syncFSentry: file not found, obj " ++ show oid ++ " will be removed, path = " ++ show p

  mapM_ deleteObjects (o ^. childrenIds . to M.elems)
  mapM_ deleteObjects (o ^. versionsIds . to M.elems)
  mapM_ deleteObjects (o ^. copiesIds   . to M.elems)

  deleteObjM oid


syncDir :: ObjId -> [FilePath] -> Cmd ()
syncDir oid es = do
  trc $ "syncDirObj: obj = " ++ show oid ++ ", entries = " ++ show es
  o <- lookupObjM oid
  p <- objPath o
  let es' = filter (\ n -> mkName n `M.notMember` (o ^. childrenIds)) $
            filter (not . boringFilePath) $
            es
  syncParts p es'
  where
    syncParts p es1 = do
      mapM_ addRaw raws
      mapM_ addXmp xmps
      mapM_ addDxo dxos
      mapM_ addJsn jsns
      mapM_ addRgb srgb

        where
        (raws, es2) = partition rawImageFilePath  es1
        (xmps, es3) = partition xmpImageFilePath  es2
        (dxos, es4) = partition dxoImageFilePath  es3
        (jsns, es5) = partition jsonImageFilePath es4
        (srgb, es6) = partition srgbSubDirPath    es5

        addRaw n = do new <- mkFSObj p n RAW
                      adjustObjM (setParent  oid) new
                      adjustObjM (addChild (mkName n) new) oid

        addXmp n = return ()
        addDxo n = return ()
        addJsn n = return ()
        addRgb n = return ()



syncDirEntries :: ObjId -> Cmd ()
syncDirEntries oid = do
  trc $ "syncDirEntries: oid = " ++ show oid

mountFS :: Cmd ()
mountFS = do
  mp <- use mountPath
  trc $ "mountFS: mount filesystem directory at " ++ show mp
  newRoot <- mkFSObj "" "." DIR
  root .= Just newRoot  -- set new _root in state

-- ----------------------------------------

objPath :: Object -> Cmd FilePath
objPath o = do
  parentPath (o ^. parentId) (o ^. fse . filePath)

parentPath :: Maybe ObjId -> FilePath -> Cmd FilePath
parentPath Nothing    p0 = return p0
parentPath (Just oid) p0 = do
  o <- lookupObjM oid
  parentPath (o ^. parentId) (o ^. fse . filePath </> p0)

absPath :: Object -> Cmd FilePath
absPath o = do
  p <- objPath o
  b <- use mountPath
  return (b </> p)

-- ----------------------------------------

saveObjStore :: FilePath -> Cmd ()
saveObjStore p = do
  trc $ "saveobjstore: save state to " ++ show p
  bs <- J.encodePretty <$> get
  io $ if null p
       then L.putStrLn bs
       else L.writeFile p bs

-- ----------------------------------------
--
-- | make an object representing a file or a directory in the FS.
--   The file path must point to an existing and readable fs entry

mkFSObj :: FilePath -> FilePath -> ObjType -> Cmd ObjId
mkFSObj p n t = do
  trc $ "mkFSObj: create an object for path " ++ show (p </> n)
  bs     <- use mountPath
  let oid = mkObjId (bs </> p </> n)
  objMap %= insertObjMap oid (mkObjectFS t (mkFSEntry n))
  return oid

-- ----------------------------------------

mkFSEntry' :: FilePath -> ObjType -> Cmd FSEntry
mkFSEntry' p t = do
  st <- io $ X.getFileStatus p

  when ( X.isDirectory st /= (t == DIR) ) $
    abort $ "FS entry type mismatch: Object of type " ++ show t ++ " expected"

  cs <- if X.isRegularFile st
        then io $ mkFileCheckSum p
        else return zeroCheckSum

  return $ FSEntry
    { _filePath = mkName p
    , _age      = TS $ X.modificationTime st
    , _checksum = cs
    }

-- ----------------------------------------



scanFSDir :: FilePath -> Cmd [FilePath]
scanFSDir p0 = do
  trc $ "scanFSDir: reading dir " ++ show p0
  io $ readDir p0
  where
    readDir :: FilePath -> IO [FilePath]
    readDir p = do
      s  <- X.openDirStream p
      xs <- readDirEntries s
      X.closeDirStream s
      return xs
      where
        readDirEntries s = do
          e1 <- X.readDirStream s
          if null e1
            then return []
            else do
              es <- readDirEntries s
              return (e1 : es)

-- ----------------------------------------
--
-- filename classification

boringFilePath :: FilePath -> Bool
boringFilePath = matchRE boringRegex

rawImageFilePath :: FilePath -> Bool
rawImageFilePath = matchRE rawImageRegex

jpgImageFilePath :: FilePath -> Bool
jpgImageFilePath = matchRE jpgImageRegex

xmpImageFilePath :: FilePath -> Bool
xmpImageFilePath = matchRE xmpImageRegex

dxoImageFilePath :: FilePath -> Bool
dxoImageFilePath = matchRE dxoImageRegex

jsonImageFilePath :: FilePath -> Bool
jsonImageFilePath = matchRE jsonImageRegex

srgbSubDirPath :: FilePath -> Bool
srgbSubDirPath = matchRE srgbSubDirRegex

boringRegex :: Regex
boringRegex = parseRegex $ intercalate "|"
  [ "[.].*"
  , ".*~"
  , "[.]bak"
  , "tiff"
  , "dng"
  ]

rawImageRegex :: Regex
rawImageRegex =  parseRegex $ "[_A-Za-z0-9]+" ++ "([.](nef|NEF|rw2|RW2|gif|tiff|ppm|pgm|pbm))"

jpgImageRegex :: Regex
jpgImageRegex = parseRegex $ "[_A-Za-z0-9]+[.](jpg|JPG)"

xmpImageRegex :: Regex
xmpImageRegex = parseRegex $ "[_A-Za-z0-9]+[.](xmp|XMP)"

jsonImageRegex :: Regex
jsonImageRegex = parseRegex $ "[_A-Za-z0-9]+[.](json)"

dxoImageRegex :: Regex
dxoImageRegex = parseRegex $ "[_A-Za-z0-9]+" ++ "([.](nef|NEF|rw2|RW2|jpg|JPG)[.]dop)"

srgbSubDirRegex :: Regex
srgbSubDirRegex = parseRegex $ intercalate "|"
  [ "srgb[0-9]*"
  , "srgb-bw"
  , "[0-9]+x[0-9]+"
  , "dxo"
  , "small"
  , "web"
  , "bw"
  ]

-- ----------------------------------------

cmd1 :: Cmd ()
cmd1 = do
  mountPath .= "."
  syncFS
  saveObjStore ""


run1 :: IO (Either Msg (), ObjStore, Log)
run1 = runCmd cmd1

-- ----------------------------------------

data ObjStore'    = ObjStore'
                    { _osMap   :: !ObjMap'
                    , _osRoot  :: !ObjId
                    , _osMount :: !Name
                    }

newtype ObjMap'   = ObjMap' (M.Map ObjId Object')

data Object'      = ImgObject
                    { _objName     :: !Name
                    , _objParent   :: !ObjId
                    , _imgParts    :: !Parts
                    }
                  | DirObject
                    { _objName     :: !Name
                    , _objParent   :: !ObjId
                    , _dirAge      :: !TimeStamp
                    , _dirContent  :: !(Map Name ObjId)
                    }
                  | ColObject
                    { _objName     :: !Name
                    , _objParent   :: !ObjId
                    , _colContent  :: ![PartId]
                    }

data Parts        = Parts (Map PartName FSentry)

type PartName     = Name

data PartId       = PartId
                    { _ptObj  :: !ObjId
                    , _ptName :: !PartName
                    }

data FSentry      = FSE
                    { _fn :: !PartName
                    , _ft :: !FStype
                    , _ts :: !TimeStamp
                    , _cs :: !CheckSum
                    }

data FStype       = FSraw    | FSmeta   | FSjson  | FSjpg | FSimg | FScopy
                  | FSimgdir | FSjpgdir | FSother | FSboring

-- ----------------------------------------
--
-- basic ops for the object' store

deriving instance Show ObjStore'

emptyObjStore' :: ObjStore'
emptyObjStore' =
  ObjStore'
  { _osMap   = emptyObjMap'
  , _osRoot  = emptyObjId
  , _osMount = emptyName
  }

-- lenses

osMap :: Lens' ObjStore' ObjMap'
osMap k s = fmap (\ new -> s { _osMap = new }) (k (_osMap s) )

osRoot :: Lens' ObjStore' (Maybe ObjId)
osRoot = root' . objId2Maybe
  where
    root' :: Lens' ObjStore' ObjId
    root' k s = fmap (\ new -> s { _osRoot = new }) (k (_osRoot s) )

osMount :: Lens' ObjStore' String
osMount = mountPath' . name2string
  where
    mountPath' :: Lens' ObjStore' Name
    mountPath' k s = fmap (\ new -> s { _osMount = new }) (k (_osMount s) )

instance ToJSON ObjStore' where
  toJSON o = J.object
    [ "osMap"   J..= _osMap   o
    , "osRoot"  J..= _osRoot  o
    , "osMount" J..= _osMount o
    ]

instance FromJSON ObjStore' where
  parseJSON = withObject "ObjStore'" $ \ o ->
    ObjStore'
    <$> o .: "osMap"
    <*> o .: "osRoot"
    <*> o .: "osMount"

-- ----------------------------------------
--
-- basic ops for the object' map

emptyObjMap' :: ObjMap'
emptyObjMap' = ObjMap' M.empty

deriving instance Show ObjMap'

isoObjMap' :: Iso' ObjMap' (M.Map ObjId Object')
isoObjMap' = iso (\ (ObjMap' om) -> om) ObjMap'

instance ToJSON ObjMap' where
  toJSON (ObjMap' om) = toJSON $ M.toList om

instance FromJSON ObjMap' where
  parseJSON j = (ObjMap' . M.fromList) <$> parseJSON j

lookupObjMap' :: ObjId -> ObjMap' -> Maybe Object'
lookupObjMap' oid (ObjMap' om) = M.lookup oid om

insertObjMap' :: ObjId -> Object' -> ObjMap' -> ObjMap'
insertObjMap' oid obj (ObjMap' om) = ObjMap' $ M.insert oid obj om

deleteObjMap' :: ObjId -> ObjMap' -> ObjMap'
deleteObjMap' oid (ObjMap' om) = ObjMap' $ M.delete oid om

{--
memberObjM :: ObjId -> Cmd Bool
memberObjM oid = uses (objMap . isoObjMap') (maybe False (const True) . M.lookup oid)
-- -}

memberObj'M :: ObjId -> Cmd' Bool
memberObj'M oid =
  uses (osMap . isoObjMap') (M.member oid)

lookupObj'M :: ObjId -> Cmd' Object'
lookupObj'M oid = do
  res <- uses (osMap . isoObjMap') (M.lookup oid)
  case res of
    Nothing -> abort $ "lookupObj'M: object not found " ++ show oid
    Just o  -> return o

{-
checkObjM :: (Object -> Bool) -> ObjId -> Cmd Object
checkObjM p oid = do
  o <- lookupObjM oid
  if p o
    then return o
    else abort $ "checkObjM: wrong object type for object " ++ show o
--}
insertObj'M :: ObjId -> Object' -> Cmd' ()
insertObj'M oid o = do
  -- insert object
  osMap . isoObjMap' %= M.insert oid o

  -- insert reference in parent object
  case o ^. objParent . objId2Maybe of
    Nothing
      -> return ()
    Just pid
      -> adjustObj'M (dirContent . at (o ^. objName) .~ Just oid) pid

deleteObj'M :: ObjId -> Cmd' ()
deleteObj'M oid = do
  -- delete reference in parent dir
  o <- lookupObj'M oid
  case o ^. objParent . objId2Maybe of
    Nothing
      -> return ()
    Just pid
      -> adjustObj'M (dirContent . at (o ^. objName) .~ Nothing) pid

  -- delete oid
  osMap . isoObjMap' %= M.delete oid


adjustObj'M :: (Object' -> Object') -> ObjId -> Cmd' ()
adjustObj'M f oid = osMap . isoObjMap' %= M.adjust f oid


-- ----------------------------------------

o1' :: Object'
o1' = (mkImgObject (mkName "abc")) {_imgParts = pm1}
  where
    pm1 = Parts $ M.fromList [(n1, FSE n1 FSraw zeroTimeStamp zeroCheckSum)]
    n1 = mkName "abc.nef"

o2' :: Object'
o2' = (mkDirObject "dir") {_dirContent = cm1}
  where
    cm1 = M.fromList [(n1, emptyObjId)]
    n1 = mkName "subdir"


mkImgObject :: Name -> Object'
mkImgObject n = ImgObject n emptyObjId emptyParts

mkDirObject :: FilePath -> Object'
mkDirObject n = DirObject (mkName n) emptyObjId zeroTimeStamp M.empty

mkColObject :: Name -> Object'
mkColObject n = ColObject n emptyObjId []

deriving instance Show Object'

instance ToJSON Object' where
  toJSON o@ImgObject{} = J.object
    [ "Object'"     J..= ("ImgObject" :: String)
    , "objName"     J..= _objName     o
    , "objParent"   J..= _objParent   o
    , "imgParts"    J..= _imgParts    o
    ]
  toJSON o@DirObject{} = J.object
    [ "Object'"     J..= ("DirObject" :: String)
    , "objName"     J..= _objName     o
    , "objParent"   J..= _objParent   o
    , "dirAge"      J..= _dirAge      o
    , "dirContent"  J..= (M.toList $ _dirContent o)
    ]
  toJSON o@ColObject{} = J.object
    [ "Object'"     J..= ("ColObject" :: String)
    , "objName"     J..= _objName     o
    , "objParent"   J..= _objParent   o
    , "colContent"  J..= _colContent  o
    ]

instance FromJSON Object' where
  parseJSON = withObject "Object'" $ \ o ->
    do t <- o .: "Object'"
       case t :: String of
         "ImgObject" ->
           ImgObject <$> (o .: "objName")
                     <*> (o .: "objParent")
                     <*> (o .: "imgParts")
         "DirObject" ->
           DirObject <$> (o .: "objName")
                     <*> (o .: "objParent")
                     <*> (o .: "dirAge")
                     <*> (M.fromList <$> (o .: "dirContent"))
         "ColObject" ->
           ColObject <$> (o .: "objName")
                     <*> (o .: "objParent")
                     <*> (o .: "colContent")
         _ -> mzero

objName :: Lens' Object' Name
objName k o = fmap (\ new -> o {_objName = new}) (k (_objName o))

objParent :: Lens' Object' ObjId
objParent k o = fmap (\ new -> o {_objParent = new}) (k (_objParent o))

imgObject :: Prism' Object' Object'
imgObject = prism id
  (\ o -> case o of
      ImgObject{} -> Right o
      _           -> Left  o
  )

dirObject :: Prism' Object' Object'
dirObject = prism id
  (\ o -> case o of
      DirObject{} -> Right o
      _           -> Left  o
  )

colObject :: Prism' Object' Object'
colObject = prism id
  (\ o -> case o of
      ColObject{} -> Right o
      _           -> Left  o
  )

imgParts :: Traversal' Object' Parts
imgParts = imgObject . imgParts'
  where
    imgParts' k o =fmap (\ new -> o {_imgParts = new}) (k (_imgParts o))

dirAge :: Traversal' Object' TimeStamp
dirAge = dirObject . dirAge'
  where
    dirAge' k o =fmap (\ new -> o {_dirAge = new}) (k (_dirAge o))

dirContent :: Traversal' Object' (Map Name ObjId)
dirContent = dirObject . dirContent'
  where
    dirContent' k o = fmap (\ new -> o {_dirContent = new}) (k (_dirContent o))

colContent :: Traversal' Object' [PartId]
colContent = colObject . colContent'
  where
    colContent' k o =fmap (\ new -> o {_colContent = new}) (k (_colContent o))

-- ----------------------------------------

emptyParts :: Parts
emptyParts = Parts M.empty

mkParts :: [FSentry] -> Parts
mkParts = view $ from isoParts

deriving instance Show Parts

isoParts :: Iso' Parts [FSentry]
isoParts =
  iso (\ (Parts pm) -> pm) Parts
  .
  isoMapElems _fn

instance ToJSON Parts where
  toJSON = toJSON . view isoParts

instance FromJSON Parts where
  parseJSON j = mkParts <$> parseJSON j

instance Monoid Parts where
  mempty = emptyParts

  Parts m1 `mappend` Parts m2 =
    Parts $ M.mergeWithKey combine only1 only2 m1 m2
    where
      only1 = const M.empty
      only2 = id
      combine _k e1 e2
        | _ts e1 > _ts e2 = Just e1
        | otherwise       = Just e2

-- ----------------------------------------

mkPartId :: ObjId -> PartName -> PartId
mkPartId = PartId

deriving instance Show PartId

instance ToJSON PartId where
  toJSON = toJSON . (\ (PartId oid n) -> (oid, n))

instance FromJSON PartId where
  parseJSON j = uncurry mkPartId <$> parseJSON j

ptObj :: Lens' PartId ObjId
ptObj k p = fmap (\ new -> p {_ptObj = new}) (k (_ptObj p))

ptName :: Lens' PartId PartName
ptName k p = fmap (\ new -> p {_ptName = new}) (k (_ptName p))

-- ----------------------------------------

deriving instance Show FSentry

instance ToJSON FSentry where
  toJSON o = J.object
    [ "fn"   J..= _fn o
    , "ft"   J..= _ft o
    , "ts"   J..= _ts o
    , "cs"   J..= _cs o
    ]

instance FromJSON FSentry where
  parseJSON = withObject "FSentry" $ \ o ->
    FSE <$> (o .: "fn") <*> (o .: "ft")
        <*> (o .: "ts") <*> (o .: "cs")

-- ----------------------------------------

deriving instance Eq   FStype
deriving instance Ord  FStype
deriving instance Show FStype
deriving instance Read FStype

instance ToJSON FStype where
  toJSON = toJSON . show

instance FromJSON FStype where
  parseJSON o = read <$> parseJSON o

hasFStype :: (FStype -> Bool) -> (Name, (Name, FStype)) -> Bool
hasFStype p (_, (_, t)) = p t

-- ----------------------------------------

filePathToFStype :: FilePath -> (Name, FStype)
filePathToFStype path = fromMaybe (mkName path, FSother) $
  parse (mk1 boringName) FSboring
  <|>
  parse (mk1 baseName  ++ rawExt)  FSraw
  <|>
  parse (mk1 baseName' ++ imgExt)  FSimg
  <|>
  parse (mk1 baseName  ++ metaExt) FSmeta
  <|>
  parse (mk1 baseName  ++ jsonExt) FSjson
  <|>
  parse (mk1 jpgdirName) FSjpgdir
  <|>
  parse (mk1 imgdirName) FSimgdir
  <|>
  parse (jpgdirPre ++ mk1 baseName ++ jpgExt)  FSjpg
  where
    parse re' c
      | null rest = partRes res
      | otherwise = Nothing
      where
        (res, rest) = splitSubex re' path

        partRes [("1", base)] =
          Just (mkName base, c)

        partRes _ = Nothing

    mk1  e = "({1}(" ++ e ++ "))"

    baseName  = "[-._A-Za-z0-9]+"
    baseName' = "[-._A-Za-z0-9]+"
    rawExt    = "[.](nef|NEF)"
    imgExt    = "[.](jpp|JPG|gif|tiff|ppm|pgm|pbm)"
    metaExt   = "[.](xmp|((nef|NEF|rw2|RW2|jpg|JPG)[.]dxo))"
    jsonExt   = "[.](json)"
    jpgExt    = "[.](jpg|JPG)"

    imgdirName = "[-_A-Za-z0-9]+" -- no do
    jpgdirName =
      "("
      ++ ( intercalate "|"
           [ "srgb[0-9]*"
           , "srgb-bw"
           , "[0-9]+x[0-9]+"
           , "dxo"
           , "small"
           , "web"
           , "bw"
           ]
         )
      ++ ")"

    jpgdirPre =
      "(" ++ jpgdirName ++ "/)?"

    boringName = intercalate "|"
      [ "[.].*"
      , ".*~"
      , "tmp.*"
      , ".*[.](bak|old|tiff|dng)"
      ]

-- ----------------------------------------

type Cmd' = Action Env ObjStore'

runCmd' :: Cmd' a -> IO (Either Msg a, ObjStore', Log)
runCmd' cmd = runAction cmd Env emptyObjStore'

syncFS' :: Cmd' ()
syncFS' = do
  notMounted <- uses osRoot isNothing -- asks _osRoot
  when notMounted mountFS'
  root <- uses osRoot fromJust
  syncFSEntry' root
  return ()

syncFSEntry' :: ObjId -> Cmd' ()
syncFSEntry' oid = do
  o <- lookupObj'M oid
  trc $ "syncFSentry': syncing oid " ++ show oid ++ " => " ++ show o
  case o of
    ImgObject{} -> do
      p <- objPath' o >>= fsPath'
      trc $ "syncFSentry: syncing image " ++ show p
      trc $ "syncFSentry: What TODO ?"


    DirObject{_dirAge = t0} -> do
      p <- objPath' o >>= fsPath'
      trc $ "syncFSentry: syncing dir " ++ show p
      s <- fsDirStat p
      case s of
        Nothing -> do
          warn $ "syncFSentry: delete missing dir object "
                 ++ show p ++ " (" ++ show oid ++ ")"
          deleteObj'M oid

        Just status -> do
          when (fsTimeStamp status > t0) $ do
            trc $ "syncFSentry: dir has changed syncing dir" ++ p

            syncDirEntries' oid

      o'new <- lookupObj'M oid
      when (M.null (o'new ^. dirContent)) $ do
        warn $ "syncFSentry: delete empty dir " ++ show oid ++ ", " ++ show p
        deleteObj'M oid

    ColObject{} -> do
      p <- objPath' o
      trc $ "syncFSentry: col object ignored " ++ show p
      trc $ "syncFSentry: What TODO ?"

syncDirEntries' :: ObjId -> Cmd' ()
syncDirEntries' oid = do
  p <- oidPath oid
  trc $ "syncDirEntries': syncing entries of dir " ++ show p
  es <- parseFSDir p
  trc $ "syncDirEntries': entries found " ++ show es

  let (others, rest) =
        partition (hasFStype (== FSother)) es
  mapM_ (\ n -> warn $ "syncDirEntries': entry ignored " ++ show (fst n)) others

  let (subdirs, rest2) =
        partition (hasFStype (== FSimgdir)) rest
  mapM_ (syncSubDir oid p) (subdirs ^.. traverse . _1 . name2string)

  let (imgfiles, rest3) =
        partition (hasFStype (`elem` [ FSraw, FSmeta, FSjson
                                     , FSjpg, FSimg,  FScopy
                                     ])) rest2
  trc $ "syncDirEntries: imgfiles " ++ show imgfiles

  mapM_ (syncImgFile oid p) (groupBy (fst . snd) imgfiles)

  trc $ "syncDirEntries: files ignored " ++ show rest3
  return ()

syncImgFile :: ObjId -> FilePath -> [(Name, (Name, FStype))] -> Cmd' ()
syncImgFile pid ppath xs = do
  trc $ "syncImgFile: syncing img " ++ show p
  trc $ "syncImgFile: syncing parts " ++ show ps
  ex <- memberObj'M oid

  when (not ex) $ do
      insertObj'M oid ((mkImgObject n0)
                       {_objParent = pid}
                      )

  adjustObj'M ( mergeParts $
                Parts $
                M.fromList $
                map (\ (n', t') ->
                      (n', FSE n' t' zeroTimeStamp zeroCheckSum )
                    ) ps
              ) oid

  syncParts oid ppath
  where
    p   = ppath </> n
    oid = mkObjId p
    n   = n0 ^. name2string
    n0  = xs ^. to head . _2 . _1
    ps  = xs &  traverse %~ (id *** snd)

    mergeParts :: Parts -> Object' -> Object'
    mergeParts m'new o = o & imgParts %~ (<> m'new)

syncParts :: ObjId -> FilePath -> Cmd' ()
syncParts oid ppath = do
  trcObj oid $ "syncParts: syncing img parts for "
  trc $ "syncParts: TODO"


syncSubDir :: ObjId -> FilePath -> FilePath -> Cmd' ()
syncSubDir pid ppath n = do
  trc $ "syncSubDir: " ++ show pid ++ ", " ++ show ppath ++ ", " ++ show n
  ex <- memberObj'M oid
  if ex
     then syncFSEntry' oid  -- dir already there
     else do
       s <- fsDirStat p
       case s of
         Nothing -> do
           warn $ "syncSubDir: fsentry ignored, not a dir " ++ show p
         Just status -> do  -- new dir
           trc $ "syncSubDir: new directory " ++ show p
           insertObj'M oid ((mkDirObject n)
                            {_objParent = pid}
                           )
           adjustObj'M (dirContent . at n0 .~ Just oid) pid

           syncFSEntry' oid

  where
    p   = ppath </> n
    n0  = mkName n
    oid = mkObjId p

parseFSDir :: FilePath -> Cmd' [(Name, (Name, FStype))]
parseFSDir p = do
  (es, jpgdirs)  <- classifyNames <$> scanFSDir' p
  jss <- mapM
           (parseFSjpgdir p)                     -- process jpg subdirs
           (jpgdirs ^.. traverse . _1 . name2string) -- (map (fromName . fst) jpgdirs)
  return $ es ++ concat jss
  where
    classifyNames =
      partition (hasFStype (/= FSjpgdir))  -- select jpg img subdirs
      .
      filter    (hasFStype (/= FSboring))  -- remove boring stuff
      .
      map (\ n -> (mkName n, filePathToFStype n))

parseFSjpgdir :: FilePath -> FilePath -> Cmd' [(Name, (Name, FStype))]
parseFSjpgdir p d =
  classifyNames <$> scanFSDir' (p </> d)
  where
    classifyNames =
      filter (\ n -> (n ^. _2 . _2) == FSjpg)
      .
      map (\ n -> (mkName (d </> n), filePathToFStype n))

scanFSDir' :: FilePath -> Cmd' [FilePath]
scanFSDir' p0 = do
  trc $ "scanFSDir': reading dir " ++ show p0
  res <- io $ readDir p0
  trc $ "scanFSDir': result is " ++ show res
  return res
  where
    readDir :: FilePath -> IO [FilePath]
    readDir p = do
      s  <- X.openDirStream p
      xs <- readDirEntries s
      X.closeDirStream s
      return xs
      where
        readDirEntries s = do
          e1 <- X.readDirStream s
          if null e1
            then return []
            else do
              es <- readDirEntries s
              return (e1 : es)

fsDirStat :: FilePath -> Cmd' (Maybe FileStatus)
fsDirStat p = do
  ex <- io $ X.fileExist p
  if ex
    then do
      status <- io $ X.getFileStatus p
      if X.isDirectory status
        then do
          return $ Just status
        else
          return Nothing
    else
      return Nothing


{- }

  x <- io $ X.fileExist p
  if x
    then do
      status <- io $ X.getFileStatus p
      let newtime = TS $ X.modificationTime status
      let oldtime = o ^. fse .age
      let update  = newtime > oldtime
      when update $ adjustObjM (fse . age .~ newtime) oid

      case o ^. otype of
        DIR -> do
          trc $ "syncFSentry: update directory"
          when update $ do
            trc $ "syncFSentry: read directory entries"
            es <- scanFSDir p
            trc $ "syncFSentry: dir contents " ++ show es
            syncDir oid es

          syncDirEntries oid

        _oty -> do
          trc $ "syncFSentry: update file"
          when update $ do
            trc $ "syncFSentry: file has changed"

    else do
      warn $ "syncFSentry: file object not found, will be removed " ++ show p
      deleteObjects oid
-- -}

setMountPath :: FilePath -> Cmd' ()
setMountPath p =
  osMount .= p

mountFS' :: Cmd' ()
mountFS' = do
  bs <- use osMount
  trc $ "mountFS': mount filesystem directory at " ++ show bs
  newRoot <- mkDir "" "."
  osRoot .= Just newRoot  -- set new _root in state

saveObjStore' :: FilePath -> Cmd' ()
saveObjStore' p = do
  trc $ "saveobjstore': save state to " ++ show p
  bs <- J.encodePretty <$> get
  io $ if null p
       then L.putStrLn bs
       else L.writeFile p bs

-- ----------------------------------------
--
-- | make an object representing a file or a directory in the FS.
--   The file path must point to an existing and readable fs entry

mkDir :: FilePath -> FilePath -> Cmd' ObjId
mkDir p n = do
  trc $ "mkDir: create an object for path " ++ show (p </> n)
  let oid = mkObjId (p </> n)
  insertObj'M oid (mkDirObject n)
  return oid

-- ----------------------------------------

partPath' :: PartId -> Cmd' FilePath
partPath' p = do
  o <- lookupObj'M (p ^.ptObj)
  parentPath' (o ^. objParent . objId2Maybe) (p ^. ptName . name2string)

oidPath :: ObjId -> Cmd' FilePath
oidPath =
  lookupObj'M >=> objPath'

objPath' :: Object' -> Cmd' FilePath
objPath' o = do
  parentPath' (o ^. objParent . objId2Maybe) (o ^. objName . name2string)

parentPath' :: Maybe ObjId -> FilePath -> Cmd' FilePath
parentPath' oid0  p0 =
  maybe
  (return p0)
  (\ oid -> do
      o <- lookupObj'M oid
      parentPath' (o ^. objParent . objId2Maybe) (o ^. objName . name2string </> p0)
  )
  oid0

fsPath' :: FilePath -> Cmd' FilePath
fsPath' p = do
  b <- use osMount
  return (b </> p)

-- ----------------------------------------
--
-- useful (?) glasses

-- prism for parsing/printing JSON

_JSON :: (ToJSON a, FromJSON a) => Prism' L.ByteString a
_JSON = prism' J.encodePretty J.decode

-- an iso for converting between maps and list of pairs

isoMapList :: Ord a => Iso' (Map a b) ([(a, b)])
isoMapList = iso M.toList M.fromList

-- an iso for converting a list of elemets into a map,
-- the key function estracts the keys of the elements

isoMapElems :: Ord k => (e -> k) -> Iso' (Map k e) [e]
isoMapElems key = iso M.elems (M.fromList . map (\ e -> (key e, e)))

-- a prism for filtering

is :: (a -> Bool) -> Prism' a a
is p = prism id (\ o -> (if p o then Right else Left) o)

-- ----------------------------------------

trcObj :: ObjId -> String -> Cmd' ()
trcObj oid msg = do
  p <- oidPath oid
  trc $ msg ++ " " ++ show p

-- ----------------------------------------

rrr :: IO (Either Msg (), ObjStore', Log)
rrr = runCmd' $ do
  -- wd <- exec "pwd" []
  -- setMountPath =<< (head . lines  <$> execProcess "pwd" [] "")
  setMountPath =<< (io $ X.getWorkingDirectory)
  mountFS'
  r <- uses osRoot fromJust
  o <- lookupObj'M r
  p <- objPath' o
  a <- fsPath' p
  trc $ "root path = " ++ show p ++ " (fspath = " ++ a ++ ")"
  syncFS'
  saveObjStore' ""

-- ----------------------------------------

data RefTree n a = DT a (Map a (n a))

data UpLink n a = UL a (n a)

type DirTree n a = RefTree (UpLink n) a

data DirNode a = F TimeStamp
               | D (Map Name a)

type FSwithNames = DirTree DirNode Name
type FSs = DirTree DirNode String

type FSwithIds = DirTree DirNode ObjId

deriving instance (Show a, Show (n a)) => Show (RefTree n a)
deriving instance (Show a, Show (n a)) => Show (UpLink  n a)
deriving instance (Show a) => Show (DirNode a)

instance Functor DirNode where
  fmap _ (F ts) = F ts
  fmap f (D m)  = D $ M.map f m

instance Functor n => Functor (UpLink n) where
  fmap f (UL x t) = UL (f x) (fmap f t)

fmap' :: (Functor n, Ord b) => (a -> b) -> RefTree n a -> RefTree n b
fmap' f (DT r t) =
  DT (f r) ( M.foldrWithKey'
             (\ k v acc -> M.insert (f k) (fmap f v) acc)
             M.empty
             t
           )

root' :: Lens' (RefTree n a) a
root' k (DT r m) = fmap (\ new -> DT new m) (k r)

entr' :: Lens' (RefTree n a) (Map a (n a))
entr' k (DT r m) = fmap (\ new -> DT r new) (k m)

upln' :: Lens' (UpLink n a) a
upln' k (UL r n) = fmap (\ new -> UL new n) (k r)

node' :: Lens' (UpLink n a) (n a)
node' k (UL r n) = fmap (\ new -> UL r new) (k n)

isD :: Prism' (DirNode a) (DirNode a)
isD = is (\ d -> case d of
             D _ -> True
             _   -> False
         )

d' k (D m) = fmap (\ new -> D new) (k m)

ins' :: Ord a => (a -> n a -> n a) -> a -> a -> n a -> DirTree n a -> DirTree n a
ins' addChild' p r n rt =
  rt & entr' . at r .~ Just (UL p n)                -- add the child
     & entr' . at p . _Just . node' %~ addChild' r  -- insert in parent


addentr' :: Name -> a -> DirNode a -> DirNode a
addentr' nm r = isD . d' . at nm .~ Just r

-- addentr' nm r (D m) = D $ M.insert nm r m
-- addentr' _ _ f = f

insFS' :: Ord a => Name -> a -> a -> DirNode a -> DirTree DirNode a -> DirTree DirNode a
insFS' nm = ins' (addentr' nm)

u1 :: FSs
u1 = undefined

-- ----------------------------------------
