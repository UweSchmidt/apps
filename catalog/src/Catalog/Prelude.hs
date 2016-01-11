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

import Control.Lens hiding (children)
import Control.Arrow (first)
import Control.Applicative
import           Control.Monad.RWSErrorIO
import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)
import           Data.Foldable
import           Data.Map.Strict (Map)
-- import           Data.Monoid
import Data.List (intercalate, partition)
import           Data.Word
import           Data.Aeson hiding (Object, (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import Data.Bits
import Data.String
import Data.Maybe
import System.FilePath ((</>))
import qualified System.Posix as X
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map.Strict as M
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Digest.Murmur64 as MM
-- import           GHC.Generics
import qualified Data.Text as T

-- ----------------------------------------

data ObjStore     = ObjStore
                    { _objMap    :: ! ObjMap
                    , _root      :: ! ObjId
                    , _mountPath :: ! Name
                    }

newtype ObjMap    = ObjMap (M.Map ObjId Object)

newtype ObjId     = ObjId Word64

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

newtype Name      = Name B.ByteString
newtype TimeStamp = TS X.EpochTime
newtype CheckSum  = CS Word64
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
root = root' . isoMaybeObjId
  where
    root' :: Lens' ObjStore ObjId
    root' k s = fmap (\ new -> s { _root = new }) (k (_root s) )

mountPath :: Lens' ObjStore String
mountPath = mountPath' . isoName
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
-- basic ops for object ids

emptyObjId :: ObjId
emptyObjId = toObjId 0

nullObjId :: ObjId -> Bool
nullObjId (ObjId i) = i == 0

mkObjId :: MM.Hashable64 a => a -> ObjId
mkObjId = ObjId . MM.asWord64 . MM.hash64

fromObjId :: ObjId -> Integer
fromObjId (ObjId w) = fromIntegral w

toObjId :: Integer -> ObjId
toObjId = ObjId . fromIntegral

isoObjIdInteger :: Iso' ObjId Integer
isoObjIdInteger = iso fromObjId toObjId

isoMaybeObjId :: Iso' ObjId (Maybe ObjId)
isoMaybeObjId =
  iso (\ i -> if nullObjId i
              then Nothing
              else Just i
      )
      (fromMaybe emptyObjId)

deriving instance Eq   ObjId
deriving instance Ord  ObjId
deriving instance Show ObjId

instance ToJSON ObjId where
  toJSON = toJSON . fromObjId

instance FromJSON ObjId where
  parseJSON o = toObjId <$> parseJSON o

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
parentId = parent . relSingle . isoMaybeObjId

versions :: Lens' Object RelVal
versions = orels . relAt VERSIONS

versionsIds :: Lens' Object (Map Name ObjId)
versionsIds = versions . relUnordered

orig :: Lens' Object RelVal
orig = orels . relAt ORIG

origId :: Lens' Object (Maybe ObjId)
origId = orig . relSingle .isoMaybeObjId

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
filePath = filePath' . isoName
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
-- basic ops for names (UTF8 encoded strict bytestrings)

emptyName :: Name
emptyName = mkName ""

mkName :: String -> Name
mkName = Name . B.pack . UTF8.encode

nullName :: Name -> Bool
nullName (Name n) = B.null n

fromName :: Name -> String
fromName (Name fsn) = UTF8.decode . B.unpack $ fsn

isoName :: Iso' Name String
isoName = iso fromName mkName

deriving instance Eq   Name
deriving instance Ord  Name

instance IsString Name where
  fromString = mkName

instance Show Name where
  show = show . fromName

instance ToJSON Name where
  toJSON = toJSON . fromName

instance FromJSON Name where
  parseJSON (J.String t) = return (mkName . T.unpack $ t)
  parseJSON _            = mzero

-- ----------------------------------------
--
-- basic ops for time stamps

zeroTimeStamp :: TimeStamp
zeroTimeStamp = TS $ read "0"

isoTimeStamp :: Iso' TimeStamp String
isoTimeStamp = iso
               (\ (TS t) -> show t)
               (TS . read)
               
deriving instance Eq   TimeStamp
deriving instance Ord  TimeStamp
deriving instance Show TimeStamp

instance ToJSON TimeStamp where
  toJSON = toJSON . view isoTimeStamp

instance FromJSON TimeStamp where
  parseJSON (J.String t) = return ((view $ from isoTimeStamp) . T.unpack $ t)
  parseJSON _            = mzero

now :: MonadIO m => m TimeStamp
now = liftIO (TS <$> X.epochTime)

-- ----------------------------------------
--
-- basic ops for checksums

zeroCheckSum :: CheckSum
zeroCheckSum = CS 0

mkCheckSum :: MM.Hashable64 a => a -> CheckSum
mkCheckSum = CS . MM.asWord64 . MM.hash64

fromCheckSum :: Integral a => CheckSum -> a
fromCheckSum (CS csum) = fromIntegral csum

toCheckSum :: Integer -> CheckSum
toCheckSum = CS . fromInteger

isoCheckSum :: Iso' CheckSum Integer
isoCheckSum = iso fromCheckSum toCheckSum

deriving instance Eq CheckSum

instance Show CheckSum where
  show = ("0x" ++) . showCheckSum

showCheckSum :: CheckSum -> String
showCheckSum (CS csum) =
  toHex 16 csum []
  where
    toHex :: Int -> Word64 -> String -> String
    toHex  0  _  acc = acc
    toHex !n !w !acc = let !c = toDig (w .&. 0xF)
                       in
                         toHex (n - 1) (w `shiftR` 4) (c : acc)

    toDig :: Word64 -> Char
    toDig w
      | w < 10    = toEnum $ fromEnum w + fromEnum '0'
      | otherwise = toEnum $ fromEnum w + (fromEnum 'a' - 10)

readCheckSum :: String -> CheckSum
readCheckSum = CS . foldl' nextDig 0
  where
    nextDig !acc !c
      | '0' <= c && c <= '9' = (acc `shiftL` 4) .|. toEnum (fromEnum c - fromEnum '0')
      | otherwise            = (acc `shiftL` 4) .|. toEnum (fromEnum c - fromEnum 'a' + 10)

instance ToJSON CheckSum where
  toJSON = toJSON . showCheckSum

instance FromJSON CheckSum where
  parseJSON o = readCheckSum <$> parseJSON o

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
        then mkFileCheckSum p
        else return zeroCheckSum

  return $ FSEntry
    { _filePath = mkName p
    , _age      = TS $ X.modificationTime st
    , _checksum = cs
    }

-- ----------------------------------------

-- | compute the checksum for a simple file
-- does not work for directories

mkFileCheckSum :: FilePath -> Cmd CheckSum
mkFileCheckSum p = do
  trc $ "mkChecksum: compute checksum for " ++ p
  mkCheckSum <$> io (readFile p)


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
                    , _dirContent :: !(Map Name ObjId)
                    }
                  | ColObject
                    { _objName     :: !Name
                    , _objParent   :: !ObjId
                    , _colContent  :: ![(Name, ObjId)] 
                    }
                    
data Parts        = Parts (Map Name FSentry)

data FSentry      = FSE
                    { _fn :: !Name
                    , _ft :: !FStype
                    , _ts :: !TimeStamp
                    , _cs :: !CheckSum
                    }

data FStype = FSraw | FSmeta |FSjson | FSjpg | FSimg | FScopy

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
osRoot = root' . isoMaybeObjId
  where
    root' :: Lens' ObjStore' ObjId
    root' k s = fmap (\ new -> s { _osRoot = new }) (k (_osRoot s) )

osMount :: Lens' ObjStore' String
osMount = mountPath' . isoName
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

lookupObjM :: ObjId -> Cmd Object
lookupObjM oid = do
  res <- uses (objMap . isoObjMap') (M.lookup oid)
  case res of
    Nothing -> abort $ "lookupObjM: object not found " ++ show oid
    Just o  -> return o

checkObjM :: (Object -> Bool) -> ObjId -> Cmd Object
checkObjM p oid = do
  o <- lookupObjM oid
  if p o
    then return o
    else abort $ "checkObjM: wrong object type for object " ++ show o
--}
insertObj'M :: ObjId -> Object' -> Cmd' ()
insertObj'M oid obj = osMap . isoObjMap' %= M.insert oid obj

{-
deleteObjM :: ObjId -> Cmd ()
deleteObjM oid = objMap . isoObjMap' %= M.delete oid

adjustObjM :: (Object -> Object) -> ObjId -> Cmd ()
adjustObjM f oid = objMap . isoObjMap' %= M.adjust f oid
-- -}

-- ----------------------------------------

o1' = (mkImgObject (mkName "abc")) {_imgParts = pm1}
  where
    pm1 = Parts $ M.fromList [(n1, FSE n1 FSraw zeroTimeStamp zeroCheckSum)]
    n1 = mkName "abc.nef"
    
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
    dirContent' k o =fmap (\ new -> o {_dirContent = new}) (k (_dirContent o))

colContent :: Traversal' Object' [(Name, ObjId)]
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
isoParts = iso
           (\ (Parts pm) -> M.elems pm)
           (Parts . M.fromList . (\ es -> zip (map _fn es) es))

instance ToJSON Parts where
  toJSON = toJSON . view isoParts

instance FromJSON Parts where
  parseJSON j = mkParts <$> parseJSON j

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

-- ----------------------------------------

isoMapList :: Ord a => Iso' (Map a b) ([(a, b)])
isoMapList = iso M.toList M.fromList

-- ----------------------------------------

filePathToFStype :: FilePath -> Maybe (Name, FStype)
filePathToFStype path =
  parse (mk1 baseName  ++ rawExt)  FSraw
  <|>
  parse (mk1 baseName' ++ imgExt)  FSimg
  <|>
  parse (mk1 baseName  ++ metaExt) FSmeta
  <|>
  parse (mk1 baseName  ++ jsonExt) FSjson
  <|>
  parse (subDirPre ++ mk1 baseName ++ jpgExt)  FSjpg
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
    subDirPre =
      "(("
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
      ++ ")/)?"

-- ----------------------------------------

type Cmd' = Action Env ObjStore'

runCmd' :: Cmd' a -> IO (Either Msg a, ObjStore', Log)
runCmd' cmd = runAction cmd Env emptyObjStore'

setMountPath :: FilePath -> Cmd' ()
setMountPath p =
  osMount .= p
  
mountFS' :: Cmd' ()
mountFS' = do
  bs <- use osMount
  trc $ "mountFS': mount filesystem directory at " ++ show bs
  newRoot <- mkDir "" ""
  osRoot .= Just newRoot  -- set new _root in state

-- ----------------------------------------
--
-- | make an object representing a file or a directory in the FS.
--   The file path must point to an existing and readable fs entry

mkDir :: FilePath -> FilePath -> Cmd' ObjId
mkDir p n = do
  trc $ "mkDir: create an object for path " ++ show (p </> n)
  bs     <- use osMount
  let oid = mkObjId (bs </> p </> n)
  insertObj'M oid (mkDirObject n)
  return oid

-- ----------------------------------------

rrr = runCmd' $ do
  setMountPath "/home/uwe/Bilder/Catalog"
  mountFS'
