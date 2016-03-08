{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd.Basic
where

import           Catalog.Cmd.Types
import           Data.ImageStore
import           Data.ImageTree
import           Data.MetaData
import           Data.Prim
import           Data.RefTree

-- ----------------------------------------

-- simple monadic ops

dt :: Cmd ImgTree
dt = use theImgTree

getTree :: Getting a ImgTree a -> Cmd a
getTree l = use (theImgTree . l)

getImgName :: ObjId -> Cmd Name
getImgName i = getTree (theNode i . nodeName)

getImgParent :: ObjId -> Cmd ObjId
getImgParent i = getTree (theNode i . parentRef)

getImgVal :: ObjId -> Cmd ImgNode
getImgVal i = getTree (theNode i . nodeVal)

getImgVals :: ObjId -> Getting a ImgNode a -> Cmd a
getImgVals i l = getTree (theNode i . nodeVal . l)

getImgSubDirs :: Set ObjId -> Cmd [ObjId]
getImgSubDirs es = filterM (\ i' -> getImgVals i' (to isDIR)) (es ^. isoSetList)

-- ----------------------------------------

getRootId :: Cmd ObjId
getRootId = getTree rootRef

getRootImgDirId :: Cmd ObjId
getRootImgDirId = do
  ri <- getRootId
  getImgVals ri theRootImgDir

getRootImgColId :: Cmd ObjId
getRootImgColId = do
  ri <- getRootId
  getImgVals ri theRootImgCol

existsObjId :: ObjId -> Cmd Bool
existsObjId i =
  isJust <$> getTree (entryAt i)

lookupByName :: Name -> ObjId -> Cmd (Maybe (ObjId, ImgNode))
lookupByName n i = do
  p <- (`snocPath` n) <$> objid2path i
  lookupByPath p

lookupByPath :: Path -> Cmd (Maybe (ObjId, ImgNode))
lookupByPath p = lookupImgPath p <$> dt

-- save lookup by path

getIdNode :: String -> Path -> Cmd (ObjId, ImgNode)
getIdNode msg p = do
  mv <- lookupImgPath p <$> dt
  case mv of
    Nothing ->
      abort $ msg ++ " " ++ show (show p)
    Just res ->
      return res

-- check path not there

notTherePath :: String -> Path -> Cmd ()
notTherePath msg p = do
  exists <- isJust <$> lookupByPath p
  when exists $
    abort $ msg ++ " " ++ show (show p)

-- ----------------------------------------

-- | ref to path
objid2path :: ObjId -> Cmd Path
objid2path i = dt >>= go
  where
    go t = return (refPath i t)

-- | ref to type
objid2type :: ObjId -> Cmd String
objid2type i = getImgVal i >>= go
  where
    go e = return $ concat $
      e ^.. ( theParts      . to (const "IMG")  <>
              theDirEntries . to (const "DIR")  <>
              theImgRoot    . to (const "Root") <>
              theImgCol     . to (const "COL")
            )

objid2contNames :: ObjId -> Cmd [Name]
objid2contNames i = getImgVal i >>= go
  where
    go e
      | isIMG e =
          return (e ^. theParts . isoImgParts . traverse . theImgName . to (:[]))

      | isDIR e =
          traverse getImgName (e ^. theDirEntries . isoSetList)

      | isROOT e = let (i1, i2) = e ^. theImgRoot in do
          n1 <- getImgName i1
          n2 <- getImgName i2
          return [n1, n2]

      | otherwise =
          return []

-- ----------------------------------------
--

-- | convert an image path to a file system path
toFilePath :: Path -> Cmd FilePath
toFilePath p = do
  mp <- use theMountPath
  return $ mp ++ tailPath p ^. path2string

-- | convert a file system path to an image path
fromFilePath :: FilePath -> Cmd Path
fromFilePath f = do
  mp <- use theMountPath
  when (not (mp `isPrefixOf` f)) $
    abort $ "fromFilePath: not a legal image path " ++ show f
  r' <- getTree rootRef >>= getImgName
  return $ consPath r' (readPath $ drop (length mp) f)

-- ----------------------------------------
--
-- smart constructors

mkImg' :: (ImgNode -> Bool) -> ImgNode -> ObjId -> Name -> Cmd ObjId
mkImg' isN v i n = dt >>= go
  where
    go t = do
      (d, t') <- liftE $ mkNode isN n i v t
      theImgTree .= t'
      trcObj d "mkImg': new image node"
      return d

mkImgDir :: ObjId -> Name -> Cmd ObjId
mkImgDir = mkImg' isDIR emptyImgDir

mkImgCol :: ObjId -> Name -> Cmd ObjId
mkImgCol = mkImg' isCOL emptyImgCol

mkImg :: ObjId -> Name -> Cmd ObjId
mkImg = mkImg' isDIR emptyImg

rmImgNode :: ObjId -> Cmd ()
rmImgNode i = dt >>= go
  where
    go t = do
      t' <- liftE $ removeImgNode i t
      theImgTree .= t'

-- ----------------------------------------
--
-- simple "file system" ops

-- create a new empty subcollection

mkCollection :: Path -> Cmd ObjId
mkCollection target'path = do
  -- parent exists
  (parent'id, parent'node) <- getIdNode "mkCollection: parent doesn't exist" parent'path

  -- parent is a collection
  -- TODO exists check
  unless (isCOL parent'node) $
    abort $ "mkCollection: parent isn't a collection " ++ show (show parent'path)

  -- check collection does not yet exist
  notTherePath "mkCollection: target collection already exists" target'path

  -- create a new empty collection and append it to the parent collection
  col'id <- mkImgCol parent'id target'name
  adjustColEntries (++ [mkColColRef col'id]) parent'id
  return col'id
  where
    (parent'path, target'name) = target'path ^. viewBase

-- ----------------------------------------
--
-- basic modification commands

adjustImg :: (ImgParts -> ImgParts) -> ObjId -> Cmd ()
adjustImg = adjustNodeVal theParts

adjustDirEntries :: (Set ObjId -> Set ObjId) -> ObjId -> Cmd ()
adjustDirEntries = adjustNodeVal theDirEntries

adjustMetaData :: (MetaData -> MetaData) -> ObjId -> Cmd ()
adjustMetaData f i =
  theImgTree . theNodeVal i . theColMetaData %= f

adjustColEntries :: ([ColEntry] -> [ColEntry]) -> ObjId -> Cmd ()
adjustColEntries = adjustNodeVal theColEntries

adjustNodeVal :: Traversal' ImgNode a -> (a -> a) -> ObjId -> Cmd ()
adjustNodeVal theComp f i =
  theImgTree . theNodeVal i . theComp %= f

setSyncTime :: ObjId -> Cmd ()
setSyncTime i = do
  t <- now
  theImgTree . theNodeVal i %= syncTime t
    where
      syncTime t (DIR es _ts)    = DIR es t
      syncTime t (COL md es _ts) = COL md es t
      syncTime _ n               = n

-- ----------------------------------------
--
-- basic Cmd combinators

liftE :: Except String a -> Cmd a
liftE cmd =
  case runExcept cmd of
    Left  msg -> abort msg
    Right res -> return res

catchAll :: Cmd () -> Cmd ()
catchAll c =
  c `catchError` (\ e -> warn $ "catchAll: error caught: " ++ show e)

runDry :: String -> Cmd () -> Cmd ()
runDry msg cmd = do
  dry <- view envDryRun
  if dry
    then do
      logg (^. envDryRun) "dry-run" msg
    else do
      verbose $ msg
      cmd

-- ----------------------------------------
--
-- trace commands

trcObj :: ObjId -> String -> Cmd ()
trcObj r msg = dt >>= \ t ->
  trc $ msg ++ " " ++ show (refPath r t)

trcCmd :: Show a => Cmd a -> Cmd a
trcCmd cmd
  = do res <- cmd
       trc $ "cmd: res = " ++ show res
       return res

-- ----------------------------------------
