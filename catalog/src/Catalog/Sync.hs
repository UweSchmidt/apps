{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Sync
where

import           Catalog.FilePath
-- import           Control.Applicative
import           Control.Arrow ((***))
import           Control.Lens hiding (children)
import           Control.Lens.Util
import           Control.Monad.RWSErrorIO
import Data.Function.Util
-- import qualified Data.Aeson as J
-- import           Data.Aeson hiding (Object, (.=))
import qualified Data.Aeson.Encode.Pretty as J
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ImageTree
import           Data.List ({-intercalate,-} partition, isPrefixOf)
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
-- import           Data.Maybe
-- import           Data.Prim.CheckSum
import           Data.Prim.Name
-- import Data.ImageTree
import Data.ImageStore
import           Data.Prim.PathId
import           Data.Prim.Path
import           Data.Prim.TimeStamp
import           Data.RefTree
import           System.FilePath -- ((</>))
import           System.Posix (FileStatus)
import qualified System.Posix as X
-- import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)
import Catalog.Cmd
import           Control.Monad.Except
-- import qualified Data.Set as S

-- ----------------------------------------

saveImgStore :: FilePath -> Cmd ()
saveImgStore p = do
  trc $ "saveImgStore: save state to " ++ show p
  bs <- uses id J.encodePretty
  io $ if null p
       then L.putStrLn bs
       else L.writeFile p bs

-- ----------------------------------------
--
-- ops on current node

-- change working node
cwSet :: ObjId -> Cmd ()
cwSet i = dt >>= go
  where
    go t = do
      when (hasn't (entryAt i . _Just) t) $
        abort $ "cwSet: node not found: " ++ show i
      theWE .= i

cwSetPath :: Path -> Cmd ()
cwSetPath p =
  cwSet (mkObjId p)
  `catchError`
  (\ _e -> abort $ "cwSetPath: no such node " ++ show p)

-- | change working node to root node
cwRoot :: Cmd ()
cwRoot = dt >>= go
  where
    go t = cwSet (t ^. rootRef)

-- | change working node to parent

cwUp :: Cmd ()
cwUp =
  withCWN $ \ cwn t ->
  if isDirRoot cwn t
     then return ()
     else theWE .= t ^. theParent cwn

cwDown :: Name -> Cmd ()
cwDown d = do
  p <- flip snocPath d <$> cwnPath
  cwSetPath p

cwnType :: Cmd String
cwnType = we >>= id2type

cwnPath :: Cmd Path
cwnPath = we >>= id2path

-- | list names of elements in current node
cwnLs :: Cmd [Name]
cwnLs = we >>= id2contNames

-- | convert working node path to file system path
cwnFilePath :: Cmd FilePath
cwnFilePath = cwnPath >>= toFilePath

-- ----------------------------------------
--

-- | convert an image path to a file system path
toFilePath :: Path -> Cmd FilePath
toFilePath p = do
  mp <- use theMountPath
  return $ mp ++ tailPath p ^. path2string

-- | convert a file system path to an image path
fromFilePath :: FilePath -> Cmd Path
fromFilePath f = dt >>= go
  where
    go t = do
      mp <- use theMountPath
      when (not (mp `isPrefixOf` f)) $
        abort $ "fromFilePath: not a legal image path " ++ show f
      let f' = drop (length mp) f
      let r' = t ^. theName (t ^. rootRef)
      return $ consPath r' (readPath f')

-- | ref to path
id2path :: ObjId -> Cmd Path
id2path i = dt >>= go
  where
    go t = return (refPath i t)

-- | ref to type
id2type :: ObjId -> Cmd String
id2type i = dt >>= go
  where
    go t = return $ concat $
      t ^.. theNodeVal i
          . ( theParts  . to (const "IMG")  <>
              isImgDir  . to (const "DIR")  <>
              isImgRoot . to (const "Root") <>
              isImgCol  . to (const "COL")
            )

id2contNames :: ObjId -> Cmd [Name]
id2contNames i = dt >>= go
  where
    go t =
      return $
      t ^. theNodeVal i
         . ( theParts . isoImgParts . traverse . theImgName . to (:[])
             <>
             theDirEntries . isoSetList . traverse . name
             <>
             isImgRoot . (_1 . name <> _2 . name)
           )
      where
        name = to (\ r -> t ^. theNode r . nodeName . to (:[]))

idSyncFS :: ObjId -> Cmd ()
idSyncFS i = dt >>= go
  where
    go t
      | isIMG e = do
          trcObj i "idSyncFS: syncing image"
          warn "TODO"
          return ()

      | isDIR e = do
          trcObj i "idSyncFS: syncing image dir"
          (do s <- id2path i >>= toFilePath >>= fsDirStat
              when (fsTimeStamp s > e ^. theDirTimeStamp) $
                do trc "idSyncFS: dir has changed since last sync"
                   syncDirCont i
              checkEmptyDir i

            ) `catchError`
            (\ _e ->
              do warn $ "idSyncFS: fs dir not found " ++ show e
                 rmImgNode i
            )
          return ()

      | otherwise = return ()
      where
        e = t ^. theNodeVal i

syncDirCont :: ObjId -> Cmd ()
syncDirCont i = do
  trcObj i "syncDirCont: syncing entries in dir "
  es <- id2path i >>= toFilePath >>= parseDirCont
  trc $ "syncDirCont: entries found " ++ show es
  p  <- id2path i

  -- remove none image entries
  let (others, rest) =
        partition (hasImgType (== IMGother)) es
  mapM_ (\ n -> warn $ "syncDirCont: entry ignored " ++ show (fst n)) others

  -- process jpg subdirs
  let (subdirs, rest2) =
        partition (hasImgType (== IMGimgdir)) rest
  mapM_ (syncSubDir i p) (subdirs ^.. traverse . _1 . name2string)

  -- process files for a single image
  let (imgfiles, rest3) =
        partition (hasImgType (`elem` [ IMGraw, IMGmeta, IMGjson
                                     , IMGjpg, IMGimg,  IMGcopy
                                     ])) rest2
  trc $ "syncDirEntries: imgfiles " ++ show imgfiles
  mapM_ (syncImg i p) (groupBy (fst . snd) imgfiles)

  -- remove unknown files
  trc $ "syncDirEntries: files ignored " ++ show rest3
  return ()

syncImg :: ObjId -> Path -> [(Name, (Name, ImgType))] -> Cmd ()
syncImg ip pp xs = dt >>= go
  where
    go t = do
      trcObj ip $ "syncImg: syncing img "
      -- trc $ "syncImg: syncing parts " ++ show ps
      when notex $
        mkImg ip n >> return ()

      adjustImg (<> mkImgParts ps) new'i -- TODO
      syncParts new'i pp
      return ()

      where
        notex = hasn't (entryAt new'i . _Just) t
        new'i = mkObjId (pp `snocPath` n)
        n     = xs ^. to head . _2 . _1
        ps    = xs &  traverse %~ uncurry mkImgPart . (id *** snd)

syncSubDir :: ObjId -> Path -> FilePath -> Cmd ()
syncSubDir pid pp n = do
  trc $ "syncSubDir: " ++ show pid ++ ", " ++ show pp ++ ", " ++ show n
  trc "TODO"

  return ()

syncParts :: ObjId -> Path -> Cmd ()
syncParts i pp = dt >>= go
  where
    go t = do
      trcObj i $ "syncParts: syncing img parts for "
      trc $ "syncParts: TODO" ++ show ps
      where
        ps = t ^. theNodeVal i . theParts

checkEmptyDir :: ObjId -> Cmd ()
checkEmptyDir i = dt >>= go
  where
    go t = do
      when (nullImgDir (t ^. theNodeVal i)) $ do
        p <- id2path i
        warn $ "checkEmptyDir: image dir empty, will be removed " ++ show p
        warn "TODO"
        -- rmImgNode i


fsDirStat :: FilePath -> Cmd FileStatus
fsDirStat p = do
  ex <- io $ X.fileExist p
  when (not ex) $
    abort $ "fs entry not found " ++ show p
  st <- io $ X.getFileStatus p
  when (not $ X.isDirectory st) $
    abort $ "fs entry not a directory " ++ show p
  return st

parseDirCont :: FilePath -> Cmd [(Name, (Name, ImgType))]
parseDirCont p = do
  (es, jpgdirs)  <- classifyNames <$> scanDirCont p
  jss <- mapM
         (parseJpgDirCont p)                       -- process jpg subdirs
         (jpgdirs ^.. traverse . _1 . name2string) -- (map (fromName . fst) jpgdirs)
  return $ es ++ concat jss
  where
    classifyNames =
      partition (hasImgType (/= IMGjpgdir))  -- select jpg img subdirs
      .
      filter    (hasImgType (/= IMGboring))  -- remove boring stuff
      .
      map (\ n -> (mkName n, filePathToImgType n))

parseJpgDirCont :: FilePath -> FilePath -> Cmd [(Name, (Name, ImgType))]
parseJpgDirCont p d =
  classifyNames <$> scanDirCont (p </> d)
  where
    classifyNames =
      filter (\ n -> (n ^. _2 . _2) == IMGjpg)
      .
      map (\ n -> (mkName (d </> n), filePathToImgType n))


scanDirCont :: FilePath -> Cmd [FilePath]
scanDirCont p0 = do
  trc $ "scanDirCont: reading dir " ++ show p0
  res <- io $ readDir p0
  trc $ "scanDirCont: result is " ++ show res
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


hasImgType :: (ImgType -> Bool) -> (Name, (Name, ImgType)) -> Bool
hasImgType p (_, (_, t)) = p t

-- ----------------------------------------


ccc :: IO (Either Msg (), ImgStore, Log)
ccc = runCmd $ do
  mountPath <- io X.getWorkingDirectory
  initImgStore "archive" "collections" mountPath
  trcCmd cwnPath >> trcCmd cwnLs >> return ()
  saveImgStore ""

  refRoot <- use (theImgTree . rootRef)
  refImg  <- use (theImgTree . theNodeVal refRoot . theRootImgDir)
  cwSet refImg >> trcCmd cwnPath >> trcCmd cwnType >> return ()

  cwe <- we
  refDir1 <- mkImgDir cwe "emil"
  cwSet refDir1 >> trcCmd cwnPath >> trcCmd cwnType >> trcCmd cwnFilePath >> return ()

  cwe' <- we
  pic1 <- mkImg cwe' "pic1"
  pic2 <- mkImg cwe' "pic2"
  trcCmd cwnLs >> return ()

  cwSet pic2 >> trcCmd cwnPath >> trcCmd cwnType >> trcCmd cwnLs >> return ()
  cwe'' <- we
  (mkImg cwe'' "xxx" >> return ()) `catchError` (\ _ -> return ()) -- error

  cwRoot >> trcCmd cwnType >> trcCmd cwnLs >> trcCmd cwnPath >> return ()
  trcCmd (fromFilePath "/Users/uwe/haskell/apps/catalog/emil") >> return ()
  saveImgStore ""
