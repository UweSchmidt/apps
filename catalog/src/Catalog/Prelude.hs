{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Catalog.Prelude
       ( module Catalog.Prelude
       , module Control.Monad.RWSErrorIO
       , module Text.Regex.XMLSchema.Generic
       )
where

import Control.Lens
import Control.Arrow (first)
import Control.Applicative
import           Control.Monad.RWSErrorIO
import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, )
import           Data.Foldable
import           Data.Map.Strict (Map)
-- import           Data.Monoid
import Data.List (intercalate)
import           Data.Word
import           Data.Aeson hiding (Object, (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import Data.Bits
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
                    { _objMap   :: ! ObjMap
                    , _root      :: ! ObjID
                    , _mountPath :: ! Name
                    }

newtype ObjMap    = ObjMap (M.Map ObjID Object)

newtype ObjID     = ObjID Word64

data Object       = Object
                    { _otype :: ! ObjType
                    , _orel  :: ! ObjRels
                    , _fse   :: ! (Maybe FSEntry)
                    }

data ObjType      = DIR | RAW | JPG | COPY | XMP | EXIF

newtype ObjRels   = ObjRels (Map RelType RelVal)

data RelType      = CHILDREN | PARENT | VERSIONS | ORIG | COPIES

data RelVal       = RelSingle    ! ObjID
                  | RelOrdered   ! [(Name, ObjID)]
                  | RelUnordered ! (Map Name ObjID)
                  | RelCopies    ! (Map ImgSize ObjID)

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

mkObjStore :: FilePath -> ObjStore
mkObjStore p =
  ObjStore
  { _objMap   = emptyObjMap
  , _root      = nullObjID
  , _mountPath = mkName p
  }

-- lenses

objMap :: Lens' ObjStore ObjMap
objMap k s = fmap (\ newOm -> s { _objMap = newOm }) (k (_objMap s) )

root :: Lens' ObjStore ObjID
root k s = fmap (\ newRoot -> s { _root = newRoot }) (k (_root s) )

mountPath :: Lens' ObjStore Name
mountPath k s = fmap (\ newMountPath -> s { _mountPath = newMountPath }) (k (_mountPath s) )

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

isoObjMap :: Iso' ObjMap (M.Map ObjID Object)
isoObjMap = iso (\ (ObjMap om) -> om) ObjMap

instance ToJSON ObjMap where
  toJSON (ObjMap om) = toJSON $ M.toList om

instance FromJSON ObjMap where
  parseJSON j = (ObjMap . M.fromList) <$> parseJSON j

lookupObjMap :: ObjID -> ObjMap -> Maybe Object
lookupObjMap oid (ObjMap om) = M.lookup oid om

insertObjMap :: ObjID -> Object -> ObjMap -> ObjMap
insertObjMap oid obj (ObjMap om) = ObjMap $ M.insert oid obj om

deleteObjMap :: ObjID -> ObjMap -> ObjMap
deleteObjMap oid (ObjMap om) = ObjMap $ M.delete oid om

memberObjM :: ObjID -> Cmd Bool
memberObjM oid = uses (objMap . isoObjMap) (maybe False (const True) . M.lookup oid)

lookupObjM :: ObjID -> Cmd Object
lookupObjM oid = do
  res <- uses (objMap . isoObjMap) (M.lookup oid)
  case res of
   Nothing -> abort $ "lookupObjM: object not found " ++ show oid
   Just o  -> return o
   
checkObjM :: (Object -> Bool) -> ObjID -> Cmd Object
checkObjM p oid = do
  o <- lookupObjM oid
  if p o
    then return o
    else abort $ "checkObjM: wrong object type for object " ++ show o

insertObjM :: ObjID -> Object -> Cmd ()
insertObjM oid obj = objMap . isoObjMap %= M.insert oid obj

deleteObjM :: ObjID -> Cmd ()
deleteObjM oid = objMap . isoObjMap %= M.delete oid

adjustObjM :: (Object -> Object) -> ObjID -> Cmd ()
adjustObjM f oid = objMap . isoObjMap %= M.adjust f oid

-- ----------------------------------------
--
-- basic ops for object ids

nullObjID :: ObjID
nullObjID = toObjID 0

mkObjID :: MM.Hashable64 a => a -> ObjID
mkObjID = ObjID . MM.asWord64 . MM.hash64

fromObjID :: ObjID -> Integer
fromObjID (ObjID w) = fromIntegral w

toObjID :: Integer -> ObjID
toObjID = ObjID . fromIntegral

deriving instance Eq   ObjID
deriving instance Ord  ObjID
deriving instance Show ObjID

instance ToJSON ObjID where
  toJSON = toJSON . fromObjID

instance FromJSON ObjID where
  parseJSON o = toObjID <$> parseJSON o

-- ----------------------------------------
--
-- basic ops for object

-- makeLenses ''Object

mkObject   :: ObjType -> ObjRels -> Object
mkObject t rs = Object t rs Nothing

mkObjectFS :: ObjType -> ObjRels -> FSEntry -> Object
mkObjectFS t rs fs = Object t rs (Just fs)

deriving instance Show Object

instance ToJSON Object where
  toJSON o = J.object $
    ( case _fse o of
        Nothing  -> []
        Just fse -> [ "fse" J..= fse]
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
    <*> ( (Just <$> o .: "fse")
          <|>
          return Nothing
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
-- basic ops for object relations

emptyObjRels :: ObjRels
emptyObjRels = ObjRels M.empty

deriving instance Eq   ObjRels
deriving instance Show ObjRels

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

deriving instance Eq   RelVal
deriving instance Ord  RelVal
deriving instance Show RelVal

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

-- makeLenses ''FSEntry

deriving instance Show FSEntry

instance ToJSON FSEntry where
  toJSON (FSEntry { _filePath = p
                  , _age      = t
                  , _checksum = cs
                  }
         ) = J.object
    [ "path"     J..= toJSON p
    , "age"      J..= toJSON t
    , "checksum" J..= toJSON cs
    ]

instance FromJSON FSEntry where
  parseJSON = withObject "FSentry" $ \ o ->
    FSEntry
    <$> o .: "path"
    <*> o .: "age"
    <*> o .: "checksum"

-- ----------------------------------------
--
-- basic ops for names (UTF8 encoded strict bytestrings)

mkName :: String -> Name
mkName = Name . B.pack . UTF8.encode

fromName :: Name -> String
fromName (Name fsn) = UTF8.decode . B.unpack $ fsn

deriving instance Eq   Name
deriving instance Ord  Name

instance Show Name where
  show = fromName

instance ToJSON Name where
  toJSON = toJSON . fromName

instance FromJSON Name where
  parseJSON (J.String t) = return (mkName . T.unpack $ t)
  parseJSON _            = mzero

-- ----------------------------------------
--
-- basic ops for time stamps

deriving instance Eq   TimeStamp
deriving instance Ord  TimeStamp
deriving instance Show TimeStamp

instance ToJSON TimeStamp where
  toJSON (TS t) = toJSON . show $ t

instance FromJSON TimeStamp where
  parseJSON (J.String t) = return (TS . read . T.unpack $ t)
  parseJSON _            = mzero

-- ----------------------------------------
--
-- basic ops for checksums

emptyCheckSum :: CheckSum
emptyCheckSum = CS 0

mkCheckSum :: MM.Hashable64 a => a -> CheckSum
mkCheckSum = CS . MM.asWord64 . MM.hash64

fromCheckSum :: Integral a => CheckSum -> a
fromCheckSum (CS csum) = fromIntegral csum

toCheckSum :: Integer -> CheckSum
toCheckSum = CS . fromInteger

deriving instance Eq CheckSum

instance Show CheckSum where
  show = showCheckSum

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
  { zipSingle    :: ObjID             -> ObjID             -> Maybe ObjID
  , zipOrdered   :: [(Name, ObjID)]   -> [(Name, ObjID)]   -> Maybe [(Name, ObjID)]
  , zipUnordered :: Map Name ObjID    -> Map Name ObjID    -> Maybe (Map Name ObjID)
  , zipCopies    :: Map ImgSize ObjID -> Map ImgSize ObjID -> Maybe (Map ImgSize ObjID)
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
mergeRelVal ops = merge
  where
    merge (RelSingle    r1) (RelSingle    r2) = RelSingle    <$> zipSingle    ops r1 r2
    merge (RelOrdered   r1) (RelOrdered   r2) = RelOrdered   <$> zipOrdered   ops r1 r2
    merge (RelUnordered r1) (RelUnordered r2) = RelUnordered <$> zipUnordered ops r1 r2
    merge (RelCopies    r1) (RelCopies    r2) = RelCopies    <$> zipCopies    ops r1 r2
    merge _                 _                 = Nothing

-- ----------------------------------------
--
-- modify relations between objects

modifyObjRel :: (ObjRels -> ObjRels) -> ObjID -> ObjMap -> ObjMap
modifyObjRel f oid (ObjMap om) = ObjMap $ M.adjust f' oid om
  where
    f' o@(Object {_orel = r}) = o {_orel = f r}

setRelVal :: RelType -> RelVal -> ObjID -> ObjMap -> ObjMap
setRelVal ot ov = modifyObjRel (<> ObjRels (M.singleton ot ov))

setChildren :: [(Name, ObjID)] -> ObjID -> ObjMap -> ObjMap
setChildren = setRelVal CHILDREN . RelOrdered

setParent :: ObjID -> ObjID -> ObjMap -> ObjMap
setParent = setRelVal PARENT . RelSingle

setVersions :: Map Name ObjID -> ObjID -> ObjMap -> ObjMap
setVersions = setRelVal VERSIONS . RelUnordered

setOrig :: ObjID -> ObjID -> ObjMap -> ObjMap
setOrig = setRelVal ORIG . RelSingle

setCopies :: Map ImgSize ObjID -> ObjID -> ObjMap -> ObjMap
setCopies = setRelVal COPIES . RelCopies

-- ----------------------------------------
--
-- other stuff

filterObj :: (Object -> Bool) -> ObjID -> ObjMap -> Maybe Object
filterObj p oid om = do
  o <- lookupObjMap oid om
  if p o
    then return o
    else mzero

removeExObj :: ObjID -> ObjMap -> Maybe ObjMap
removeExObj oid (ObjMap om)
  | oid `M.member` om =
      Just $ ObjMap $ M.delete oid om
  | otherwise =
      Nothing
-- ----------------------------------------
--
-- basic file system ops


scanFSDir :: ObjID -> ObjMap -> IO [FilePath]
scanFSDir = scanFSDir' fs
  where
    fs ""        = False
    fs ('.' : _) = False
    fs p
      | last p == '~' = False
      | otherwise     = True

scanFSDir' :: (FilePath -> Bool) -> ObjID -> ObjMap -> IO [FilePath]
scanFSDir' fp oid om =
  case path of
    Nothing -> return []
    Just p  -> readDir p
  where
    path = do
      obj <- filterObj isDirObj oid om
      fse <- _fse obj
      return . fromName . _filePath $ fse

    readDir p = do
      s  <- X.openDirStream p
      xs <- readDirEntries s
      X.closeDirStream s
      return xs

    readDirEntries s = do
      e1 <- X.readDirStream s
      if null e1
        then return []
        else do es <- readDirEntries s
                return $ if fp e1
                         then e1 : es
                         else      es

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
runCmd cmd = runAction cmd Env (mkObjStore ".")

syncFS :: Cmd ()
syncFS = do
  r <- use root  -- asks _root
  when (r == nullObjID) $
    mountFS
  -- do all the rest
  return ()

mountFS :: Cmd ()
mountFS = do
  mp <- uses mountPath fromName
  trc $ "mountFS: mount filesystem directory at " ++ show mp
  newRoot <- mkFSObj "" DIR
  root .= newRoot  -- set new _root in state

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

mkFSObj :: FilePath -> ObjType -> Cmd ObjID
mkFSObj p t = do
  trc $ "mkFSObj: create an object for path " ++ show p
  bs  <- use mountPath
  fse <- mkFSEntry (fromName bs </> p) t
  objMap %= insertObjMap oid (obj fse)
  return oid
  where
    oid     = mkObjID p
    obj fse = mkObjectFS t emptyObjRels fse

-- ----------------------------------------

mkFSEntry :: FilePath -> ObjType -> Cmd FSEntry
mkFSEntry p t = do
  st <- io $ X.getFileStatus p

  when ( X.isDirectory st /= (t == DIR) ) $
    abort $ "FS entry type mismatch: Object of type " ++ show t ++ " expected"

  cs <- if X.isRegularFile st
        then mkFileCheckSum p
        else return emptyCheckSum

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


scanFSDir'' :: (FilePath -> Bool) -> ObjID -> Cmd [FilePath]
scanFSDir'' fp oid = do
  p <- path
  maybe (return []) readDir p
  where
    path = do
      obj <- checkObjM isDirObj oid
      return (fromName . _filePath <$> _fse obj)
      
    readDir :: FilePath -> Cmd [FilePath] 
    readDir p = io $ do
      s  <- X.openDirStream p
      xs <- readDirEntries s
      X.closeDirStream s
      return xs
      where
        readDirEntries s = do
          e1 <- X.readDirStream s
          if null e1
            then return []
            else do es <- readDirEntries s
                    return $ if fp e1
                             then e1 : es
                             else      es
                                   
-- ----------------------------------------

boringFilePath :: FilePath -> Bool
boringFilePath p = undefined

boringRegex :: Regex
boringRegex =
  parseRegex $
  intercalate "|"
  [ "[.].*"
  , ".*~"
  , "[.]bak"
  ]
