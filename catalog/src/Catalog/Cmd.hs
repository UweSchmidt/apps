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
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.Prelude
import           Data.RefTree

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
import           Data.ImageTree
import           Data.Prim.TimeStamp
import           System.Posix (FileStatus)
import qualified System.Posix as X
import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)
-- -}

-- ----------------------------------------

data Env = Env
  { _copyGeo :: [CopyGeo]
  , _metaSrc :: [ImgType]
  }

type CopyGeo = ((Int, Int), AspectRatio)

initEnv :: Env
initEnv = Env
  { _copyGeo = [ ((1400, 1050), Pad)
               , (( 160,  160), Pad)
               , (( 160,  120), Fix)
               ]
  , _metaSrc = [ IMGraw, IMGmeta]
  }

envCopyGeo :: Lens' Env [CopyGeo]
envCopyGeo k e = (\ new -> e {_copyGeo = new}) <$> k (_copyGeo e)

envMetaSrc :: Lens' Env [ImgType]
envMetaSrc k e = (\ new -> e {_metaSrc = new}) <$> k (_metaSrc e)

deriving instance Show Env

instance Config Env where

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
              isImgCol      . to (const "COL")
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
                  let c = e ^. isImgCol
                  r1 <- colf i c
                  r2 <- return mempty        -- TODO: extend
                  return (r1 <> r2)
              | otherwise ->
                  return mempty
-- -}

-- | A general foldMap for an image tree
--
-- 1. all children are processed by a monadic action
-- 2. the node itself is processed by a monadic action
-- 3. the results are combined with a monoid op

foldMapTree :: Monoid r =>
               (ObjId -> (ObjId, ObjId)         -> Cmd r) ->  -- fold the root node
               (ObjId -> Set ObjId              -> Cmd r) ->  -- fold an image dir node
               (ObjId -> ImgParts               -> Cmd r) ->  -- fold a single image
               (ObjId -> ()                     -> Cmd r) ->  -- fold a collection
               (ObjId                           -> Cmd r)
foldMapTree = foldMTree rootC dirC colC
  where
    rootC r1 r2 r3 = r1 <> r2 <> r3
    dirC  r1 rs    = r1 <> mconcat rs
    colC  r1       = r1

-- | A general fold for an image tree
--
-- 1. all children are processed by a monadic action
-- 2. the node itself is processed by a monadic action
-- 3. the results are combined

foldMTree :: (r1 -> r -> r -> r) ->                          -- combining ROOT res
             (r2 -> [r]    -> r) ->                          -- combining DIR  res
             (r3           -> r) ->                          -- combining COL  res
             (ObjId -> (ObjId, ObjId)         -> Cmd r1) ->  -- fold the root node
             (ObjId -> Set ObjId              -> Cmd r2) ->  -- fold an image dir node
             (ObjId -> ImgParts               -> Cmd r ) ->  -- fold a single image
             (ObjId -> ()                     -> Cmd r3) ->  -- fold a collection
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
                  let c = e ^. isImgCol
                  r1 <- colf i c
                  return (colC r1)
              | otherwise ->
                  abort $ "foldTree: illegal node"

invImages :: Cmd ()
invImages = do
  _r <- use (theImgTree . rootRef)
  return ()

remRec :: ObjId -> Cmd ()
remRec =
  foldMapTree rootF dirF imgF colF
  where
    -- do nothing with the root node
    rootF _i _ =
      return ()

    -- remove the directory, as long as it's not the top image dir
    dirF i _ = do
      pe <- getImgParent i >>= getImgVal
      when (not $ isROOT pe) $
        rmImgNode i

    -- remove the image node
    imgF i _ =
      rmImgNode i

    -- do nothing with a collection node
    colF _i _ =
      return ()



listNames :: ObjId -> Cmd String
listNames r =
  unlines <$> foldMTree rootC dirC colC rootF dirF imgF colF r
  where
    gn i = show <$> getImgName i
    ind  = map ("  " ++)

    rootC n x1 x2 = n : ind (x1 ++ x2)
    dirC  n xs    = n : ind (concat xs)
    colC  n       = n : []
    rootF i _     = gn i
    dirF  i _     = gn i
    colF  i _     = gn i
    imgF i ps     = do
      n <- gn i
      return (n : ind (ps ^. isoImgParts . traverse . theImgName . name2string . to (:[])))

listPaths' :: ObjId -> Cmd [Path]
listPaths' r =
  foldMapTree rootf dirf imgf colf r
  where
    rootf i _ = (:[]) <$> id2path i
    dirf      = rootf
    colf      = rootf
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
