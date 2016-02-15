{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Sync
where

import           Catalog.Cmd
import           Catalog.FilePath
import           Control.Arrow ((***))
import           Control.Lens hiding (children)
import           Control.Lens.Util
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Function.Util
import           Data.ImageStore
import           Data.ImageTree
import           Data.List ({-intercalate,-} partition, isPrefixOf)
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.TimeStamp
import           Data.RefTree
import qualified Data.Set as S
import           System.FilePath -- ((</>))
import           System.Posix (FileStatus)
import qualified System.Posix as X
-- import           Control.Applicative
-- import qualified Data.Aeson as J
-- import           Data.Aeson hiding (Object, (.=))
-- import qualified Data.ByteString as B
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
-- import           Data.Maybe
-- import           Data.Prim.CheckSum
-- import Data.ImageTree
-- import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)

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
            )
            `catchError`
            (\ _e ->
              do warn $ "idSyncFS: fs dir not found " ++ show e
                 rmImgNode i
            )
          return ()

      | otherwise =
          return ()
      where
        e = t ^. theNodeVal i

syncDirCont :: ObjId -> Cmd ()
syncDirCont i = do
  trcObj i "syncDirCont: syncing entries in dir "
  (subdirs, imgfiles) <- collectDirCont i
  p  <- id2path i

  cont <- id2contNames i
  let lost = filter (`notElem` (subdirs ++ (map (fst . snd . head) imgfiles))) cont

  mapM_ (remDirCont i p) lost
  mapM_ (syncSubDir i p) subdirs
  mapM_ (syncImg    i p) imgfiles

remDirCont :: ObjId -> Path -> Name -> Cmd ()
remDirCont i p n = do
  trcObj i $ "remDirCont: remove entry " ++ show n ++ " from dir"
  adjustDirEntries (S.delete new'i) i
  rmImgNode new'i
  where
    new'i = mkObjId (p `snocPath` n)

collectDirCont :: ObjId -> Cmd ([Name], [ClassifiedNames])
collectDirCont i = do
  trcObj i "collectDirCont: group entries in dir "
  fp <- id2path i >>= toFilePath
  es <- parseDirCont fp
  trc $ "collectDirCont: entries found " ++ show es

  let (others, rest) =
        partition (hasImgType (== IMGother)) es
  let (subdirs, rest2) =
        partition (hasImgType (== IMGimgdir)) rest
  let (imgfiles, rest3) =
        partition (hasImgType (`elem` [ IMGraw, IMGmeta, IMGjson
                                      , IMGjpg, IMGimg,  IMGcopy
                                      ])) rest2

  mapM_ (\ n -> warn $ "collectDirCont: other entry ignored " ++ show (fst n)) others
  realsubdirs <- filterM (isSubDir fp) subdirs

  trc $ "collectDirEntries: files ignored " ++ show rest3
  trc $ "collectDirEntries: subdirs "       ++ show realsubdirs
  trc $ "collectDirEntries: imgfiles "      ++ show imgfiles

  return ( realsubdirs ^.. traverse . _1
         , groupBy (^. _2 . _1) imgfiles
         )
  where
    isSubDir fp n =
      (const True <$> fsDirStat p)
      `catchError`
      (\ _ -> do warn $ "collectDirCont: error catched, not an image dir: " ++ show p
                 return False
      )
      where
        p = fp </> (n ^. _1 . name2string)

type ClassifiedName  = (Name, (Name, ImgType))
type ClassifiedNames = [ClassifiedName]

syncImg :: ObjId -> Path -> ClassifiedNames -> Cmd ()
syncImg ip pp xs = dt >>= go
  where
    go t = do
      trcObj ip $ "syncImg: syncing img "
      when notex $
        mkImg ip n >> return ()
      adjustImg (<> mkImgParts ps) new'i -- TODO
      -- idSyncFS new'i
      syncParts new'i pp
      where
        notex = hasn't (entryAt new'i . _Just) t
        new'i = mkObjId (pp `snocPath` n)
        n     = xs ^. to head . _2 . _1
        ps    = xs &  traverse %~ uncurry mkImgPart . (id *** snd)

syncSubDir :: ObjId -> Path -> Name -> Cmd ()
syncSubDir ip pp n = dt >>= go
  where
    go t = do
      trc $ "syncSubDir: " ++ show ip ++ ", " ++ show pp ++ ", " ++ show n
      when notex $
        mkImgDir ip n >> return ()
      idSyncFS new'i
      where
        notex = hasn't (entryAt new'i . _Just) t
        new'i = mkObjId (pp `snocPath` n)

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
        rmImgNode i


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
      filter (\ n -> (n ^. _2 . _2) `elem` [IMGjpg, IMGcopy])
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
  idSyncFS refImg
  saveImgStore ""
