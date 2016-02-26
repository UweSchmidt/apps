{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd
       ( module Catalog.Cmd
       , module Control.Monad.RWSErrorIO
       , module Control.Monad.Except
       )
where

import           Control.Lens
import           Control.Lens.Util
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.ImageTree
import           Data.ImgAction
import           Data.MetaData
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.Prelude
import           Data.Prim.TimeStamp
import           Data.RefTree
import System.Directory (removeFile)
{-}
import           Catalog.FilePath
import           Control.Applicative
import           Control.Arrow (first, (***))
import           Control.Lens.Util
import qualified Data.Aeson as J
import           Data.Aeson hiding (Object, (.=))
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Prim.CheckSum
import           Data.Prim.TimeStamp
import           System.Posix (FileStatus)
import qualified System.Posix as X
import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)
-- -}

-- ----------------------------------------

data Env = Env
  { _copyGeo :: [CopyGeo]
  , _metaSrc :: [ImgType]
  , _trc     :: Bool
  , _verbose :: Bool
  , _dryRun  :: Bool
  }

type CopyGeo = ((Int, Int), AspectRatio)

initEnv :: Env
initEnv = Env
  { _copyGeo = [ ((1400, 1050), Pad)
               , (( 160,  160), Pad)
               , (( 160,  120), Fix)
               ]
  , _metaSrc = [IMGraw, IMGimg, IMGmeta]
  , _trc     = True
  , _verbose = True
  , _dryRun  = False
  }

envCopyGeo :: Lens' Env [CopyGeo]
envCopyGeo k e = (\ new -> e {_copyGeo = new}) <$> k (_copyGeo e)

envMetaSrc :: Lens' Env [ImgType]
envMetaSrc k e = (\ new -> e {_metaSrc = new}) <$> k (_metaSrc e)

envTrc :: Lens' Env Bool
envTrc k e = (\ new -> e {_trc = new}) <$> k (_trc e)

envVerbose :: Lens' Env Bool
envVerbose k e = (\ new -> e {_verbose = new}) <$> k (_verbose e)

envDryRun :: Lens' Env Bool
envDryRun k e = (\ new -> e {_dryRun = new}) <$> k (_dryRun e)

deriving instance Show Env

instance Config Env where
  traceOn   e = e ^. envTrc
  verboseOn e = e ^. envVerbose

-- ----------------------------------------

type Cmd = Action Env ImgStore

runCmd :: Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd cmd = runAction cmd initEnv emptyImgStore

-- ----------------------------------------

initImgStore :: Name -> Name -> FilePath -> Cmd ()
initImgStore rootName colName mountPath
  = do r <- liftE $
            mkEmptyImgRoot rootName dirName colName
       put $ mkImgStore r mPath (r ^. rootRef)
  where
    dirName  = mkName $ takeFileName mountPath
    mPath    = takeDirectory mountPath

-- ----------------------------------------
--
-- simple monadic ops

we :: Cmd ObjId
we = use theWE

dt :: Cmd ImgTree
dt = use theImgTree

getTree :: Getting a ImgTree a -> Cmd a
getTree l = use (theImgTree . l)

getImgName :: ObjId -> Cmd Name
getImgName i = use (theImgTree . theNode i . nodeName)

getImgParent :: ObjId -> Cmd ObjId
getImgParent i = use (theImgTree . theNode i . parentRef)

getImgVal :: ObjId -> Cmd ImgNode
getImgVal i = use (theImgTree . theNode i . nodeVal)

getImgVals :: ObjId -> Getting a ImgNode a -> Cmd a
getImgVals i l = use (theImgTree . theNode i . nodeVal . l)

withCWN :: (ObjId -> ImgTree -> Cmd a) -> Cmd a
withCWN cmd
  = do wd <- we
       t  <- dt
       cmd wd t

liftE :: Except String a -> Cmd a
liftE cmd = cmd `andThenE` return

andThenE :: Except String a -> (a -> Cmd b) -> Cmd b
andThenE cmd f =
  case runExcept cmd of
    Left  msg -> abort msg
    Right res -> f res

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

-- | ref to path
id2path :: ObjId -> Cmd Path
id2path i = dt >>= go
  where
    go t = return (refPath i t)

-- | ref to type
id2type :: ObjId -> Cmd String
id2type i = getImgVal i >>= go
  where
    go e = return $ concat $
      e ^.. ( theParts      . to (const "IMG")  <>
              theDirEntries . to (const "DIR")  <>
              theImgRoot    . to (const "Root") <>
              theImgCol      . to (const "COL")
            )

-- ----------------------------------------
--
-- smart constructors

mkImg' :: ImgNode -> ObjId -> Name -> Cmd ObjId
mkImg' v i n = dt >>= go
  where
    go t = do
      (d, t') <- liftE $ mkImgNode n i v t
      theImgTree .= t'
      trcObj d "mkImg': new image node"
      return d

mkImgDir :: ObjId -> Name -> Cmd ObjId
mkImgDir = mkImg' emptyImgDir

mkImg :: ObjId -> Name -> Cmd ObjId
mkImg = mkImg' emptyImg

rmImgNode :: ObjId -> Cmd ()
rmImgNode i = dt >>= go
  where
    go t = do
      t' <- liftE $ removeImgNode i t
      theImgTree .= t'

adjustImg :: (ImgParts -> ImgParts) -> ObjId -> Cmd ()
adjustImg f i =
  theImgTree . theNodeVal i . theParts %= f

adjustDirEntries :: (Set ObjId -> Set ObjId) -> ObjId -> Cmd ()
adjustDirEntries f i =
  theImgTree . theNodeVal i . theDirEntries %= f

setDirSyncTime :: ObjId -> Cmd ()
setDirSyncTime i = do
  t <- now
  theImgTree . theNodeVal i . theDirSyncTime .= t

{-}
-- | process all image nodes with a monadic action
--
-- the image dir hierachy is traversed and all images are processed
-- with a monadic op

processImages :: Monoid r => (ObjId -> ImgParts -> Cmd r) -> ObjId -> Cmd r
processImages pf = process
  where
    process i = do
      -- trcObj i "processImgTree: process node "
      getImgVal i >>= go
      where
        go e
          | isIMG e =
              pf i (e ^. theParts)
          | isDIR e =
              mconcat <$> traverse process (e ^. theDirEntries . isoSetList)
          | isROOT e =
              process (e ^. theRootImgDir)
          | otherwise =
              return mempty
-- -}

-- | better(?) version of processImages
--
-- The image hierachy is traversed, but changes in the dir tree do not
-- affect the traversal

processImages :: Monoid r => (ObjId -> ImgParts -> Cmd r) -> ObjId -> Cmd r
processImages pf i0 = dt >>= process
  where
    process t = go i0
      where
        go i = do
          -- trcObj i "processImgTree: process node "
          case t ^. theNodeVal i of
            e | isIMG e ->
                  pf i (e ^. theParts)
              | isDIR e ->
                  mconcat <$> traverse go (e ^. theDirEntries . isoSetList)
              | isROOT e ->
                  go (e ^. theRootImgDir)
              | otherwise ->
                  return mempty

{-}
foldMapTree :: Monoid r =>
               (ObjId -> (ObjId, ObjId)         -> Cmd r) ->  -- fold the root node
               (ObjId -> Set ObjId              -> Cmd r) ->  -- fold an image dir node
               (ObjId -> ImgParts               -> Cmd r) ->  -- fold a single image
               (ObjId -> ()                     -> Cmd r) ->  -- fold a collection
               (ObjId                           -> Cmd r)
foldMapTree rootf dirf imgf colf i0 = dt >>= process
  where
    process t = go i0
      where
        go i = do
          trcObj i "foldMapTree: fold node"
          case t ^. theNodeVal i of
            e | isIMG e ->
                  imgf i (e ^. theParts)
              | isDIR e -> do
                  let d@(_ts, rset) = e ^. theImgDir
                  r1 <- dirf i d
                  rs <- mapM go (rset ^. isoSetList)
                  return (r1 <> mconcat rs)
              | isROOT e -> do
                  let r@(rd, rc) = e ^. theImgRoot
                  r1 <- rootf i r
                  r2 <- go rd
                  r3 <- go rc
                  return (r1 <> r2 <> r3)
              | isCOL e -> do
                  let c = e ^. theImgCol
                  r1 <- colf i c
                  r2 <- return mempty        -- TODO: extend
                  return (r1 <> r2)
              | otherwise ->
                  return mempty
-- -}

type Act r = ObjId -> Cmd r

foldMT :: (         ObjId -> ImgParts                            -> Cmd r) ->  -- IMG
          (Act r -> ObjId -> Set ObjId              -> TimeStamp -> Cmd r) ->  -- DIR
          (Act r -> ObjId -> ObjId    -> ObjId                   -> Cmd r) ->  -- ROOT
          (Act r -> ObjId -> MetaData -> [ColEntry] -> TimeStamp -> Cmd r) ->  -- COL
           Act r
foldMT imgA dirA' rootA' colA' i0 = do
  go i0
  where
    dirA  = dirA'  go
    rootA = rootA' go
    colA  = colA'  go
    go i  = do
      trcObj i $ "foldMT"
      n <- getTree (theNode i)
      case n ^. nodeVal of
        e | isIMG e ->
            imgA i (e ^. theParts)
          | isDIR e ->
              let (es, ts) = e ^. theDir in
              dirA i es ts
          | isROOT e ->
              let (dir, col) = e ^. theImgRoot in
              rootA i dir col
          | isCOL e ->
              let (md, es, ts) = e ^. theImgCol in
              colA i md es ts
          | otherwise ->
              abort "foldMT: illegal argument"

-- | A general foldMap for an image tree
--
-- 1. all children are processed by a monadic action
-- 2. the node itself is processed by a monadic action
-- 3. the results are combined with a monoid op

foldMapTree :: Monoid r =>
               (ObjId -> (ObjId, ObjId)         -> Cmd r) ->  -- fold the root node
               (ObjId -> Set ObjId              -> Cmd r) ->  -- fold an image dir node
               (ObjId -> ImgParts               -> Cmd r) ->  -- fold a single image
               (ObjId -> MetaData -> [ColEntry] -> Cmd r) ->  -- fold a collection
               (ObjId                           -> Cmd r)
foldMapTree = foldMTree rootC dirC colC
  where
    rootC r1 r2 r3 = r1 <> r2 <> r3
    dirC  r1 rs    = r1 <> mconcat rs
    colC  r1 rs    = r1 <> mconcat rs

-- | A general fold for an image tree
--
-- 1. all children are processed by a monadic action
-- 2. the node itself is processed by a monadic action
-- 3. the results are combined

foldMTree :: (r1 -> r -> r -> r) ->                          -- combining ROOT res
             (r2 -> [r]    -> r) ->                          -- combining DIR  res
             (r3 -> [r]    -> r) ->                          -- combining COL  res
             (ObjId -> (ObjId, ObjId)         -> Cmd r1) ->  -- fold the root node
             (ObjId -> Set ObjId              -> Cmd r2) ->  -- fold an image dir node
             (ObjId -> ImgParts               -> Cmd r ) ->  -- fold a single image
             (ObjId -> MetaData -> [ColEntry] -> Cmd r3) ->  -- fold a collection
             (ObjId                           -> Cmd r)
foldMTree rootC dirC      colC
          rootf dirf imgf colf i0 = dt >>= process
  where
    process t = go i0
      where
        go i = do
          -- trcObj i "foldMapTree: fold node"
          case t ^. theNodeVal i of
            e | isIMG e ->
                  imgf i (e ^. theParts)
              | isDIR e -> do
                  let es = e ^. theDirEntries
                  rs <- mapM go (es ^. isoSetList)
                  r1 <- dirf i es
                  return (dirC r1 rs)
              | isROOT e -> do
                  let r@(rd, rc) = e ^. theImgRoot
                  r2 <- go rd
                  r3 <- go rc
                  r1 <- rootf i r
                  return (rootC r1 r2 r3)
              | isCOL e -> do
                  let (md, cs, _ts) = e ^. theImgCol
                  r2 <- mapM go $ cs ^.. traverse . theColColRef
                  r1 <- colf i md cs
                  return (colC r1 r2)
              | otherwise ->
                  abort $ "foldTree: illegal node"

-- ----------------------------------------

invImages :: Cmd ()
invImages = do
  _r <- use (theImgTree . rootRef)
  return ()

-- ----------------------------------------

rmR :: ObjId -> Cmd ()
rmR = foldMT imgA dirA rootA colA
  where
    imgA i _p = rmImgNode i

    dirA go i es _ts = do
      mapM_ go (es ^. isoSetList)               -- process subdirs first
      pe <- getImgParent i >>= getImgVal        -- remode dir node
      when (not $ isROOT pe) $                  -- if it's not the top dir
        rmImgNode i

    rootA go _i dir col =
      go dir >> go col                          -- recurse into dir and col hirachy
                                                -- but don't change root
    colA go i _md cs _ts = do
      mapM_ go (cs ^.. traverse . theColColRef)
      pe <- getImgParent i >>= getImgVal        -- remove collection node
      when (not $ isROOT pe) $                  -- if it's not the top collection
        rmImgNode i

-- ----------------------------------------

rmGenFiles :: (ImgPart -> Bool) -> ObjId -> Cmd ()
rmGenFiles pp =
  foldMT imgA dirA rootA colA
  where
    imgA i ps = do                              -- remove the generated file(s)
      path <-  id2path i
      runDry ("remove metadata or image copy files for " ++ show (show path)) $ do
        mapM_ (rmj path) (ps ^. isoImgParts)
        adjustImg filterJson i
      where
        rmj path part
          | pp part = do
              fp <- toFilePath (substPathName (part ^. theImgName) path)
              io $ removeFile fp
          | otherwise =
              return ()

        filterJson pts =
          pts & isoImgParts %~ filter (not . pp)

    dirA go _i es _ts =                         -- recurse into dir entries
      mapM_ go (es ^. isoSetList)

    rootA go _i dir _col =                      -- recurse only into dir hierachy
      go dir

    colA _go _i _md _es _ts =                   -- noop for collections
      return ()

rmJSON :: ObjId -> Cmd ()
rmJSON = rmGenFiles isJSON
  where
    isJSON p = p ^. theImgType == IMGjson

rmImgCopies :: ObjId -> Cmd ()
rmImgCopies = rmGenFiles isCopy
  where
    isCopy p = p ^. theImgType == IMGcopy

rmImgCopy :: Geo -> ObjId -> Cmd ()
rmImgCopy (w, h) = rmGenFiles isCopy
  where
    isCopy p =
      p ^. theImgType == IMGcopy
      &&
      match (".*[.]" ++ show w ++ "x" ++ show h ++ "[.]jpg")
            (p ^. theImgName . name2string)

-- ----------------------------------------

listNames :: ObjId -> Cmd String
listNames r =
  unlines <$> foldMTree rootC dirC colC rootF dirF imgF colF r
  where
    gn i = show <$> getImgName i
    ind  = map ("  " ++)

    rootC n x1 x2 = n : ind (x1 ++ x2)
    dirC  n xs    = n : ind (concat xs)
    colC  n xs    = n : ind (concat xs)
    rootF i _     = gn i
    dirF  i _     = gn i
    colF  i _ _   = gn i
    imgF i ps     = do
      n <- gn i
      return (n : ind (ps ^. isoImgParts . traverse . theImgName . name2string . to (:[])))

listPaths' :: ObjId -> Cmd [Path]
listPaths' r =
  foldMapTree rootf dirf imgf colf r
  where
    f i       = (:[]) <$> id2path i
    rootf i _ = f i
    dirf  i _ = f i
    colf  i _ _  = f i
    imgf i ps = do
      pp <- getImgParent i >>= id2path
      r1 <- rootf i ps
      return ( r1
               ++
               map (pp `snocPath`)
                   (ps ^.. isoImgParts . traverse . theImgName)
             )

listPaths :: ObjId -> Cmd String
listPaths i = (unlines . map show) <$> listPaths' i

listImages' :: Cmd [(Path, [Name])]
listImages' = do
  r <- use (theImgTree . rootRef)
  processImages listImg r
  where
    listImg :: ObjId -> ImgParts -> Cmd [(Path, [Name])]
    listImg i ps = do
      p <- id2path i
      let pns = ps ^.. isoImgParts . traverse . theImgName
      return [(p, pns)]

listImages :: Cmd String
listImages = formatImages <$> listImages'
  where
    formatImages :: [(Path, [Name])] -> String
    formatImages = unlines . map (uncurry fmt)
      where
        fmt p ns = show p ++ ": " ++ intercalate ", " (map show ns)

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

{-}
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
-- -}

id2contNames :: ObjId -> Cmd [Name]
id2contNames i = getImgVal i >>= go
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
-- ops on current node

-- change working node

cwSet :: ObjId -> Cmd ()
cwSet i = do
  e <- getTree (entryAt i)
  case e of
    Nothing ->
      abort $ "cwSet: node not found: " ++ show i
    Just _ ->
      theWE .= i

cwSetPath :: Path -> Cmd ()
cwSetPath p =
  cwSet (mkObjId p)
  `catchError`
  (\ _e -> abort $ "cwSetPath: no such node " ++ show p)

-- | change working node to root node
cwRoot :: Cmd ()
cwRoot = getTree rootRef >>= cwSet

-- | change working node to parent

cwUp :: Cmd ()
cwUp = do
  ip <- we >>= getImgParent
  theWE .= ip


cwDown :: Name -> Cmd ()
cwDown d = do
  p <- flip snocPath d <$> cwPath
  cwSetPath p

cwType :: Cmd String
cwType = we >>= id2type

cwPath :: Cmd Path
cwPath = we >>= id2path

-- | list names of elements in current node
cwLs :: Cmd [Name]
cwLs = we >>= id2contNames

-- | convert working node path to file system path
cwFilePath :: Cmd FilePath
cwFilePath = cwPath >>= toFilePath

cwListPaths :: Cmd String
cwListPaths = we >>= listPaths

cwListNames :: Cmd String
cwListNames = we >>= listNames

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