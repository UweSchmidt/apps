{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Catalog.Workflow
where

import Data.Prim
import Data.ImgNode
import Data.ImgTree
import Data.Semigroup
import Catalog.Cmd
import qualified Text.Blaze.Html      as Blaze

-- ----------------------------------------

type PathPos = (Path, Pos)
type IdNode  = (ObjId, ImgNode)
type Pos     = Maybe Int

data ReqType = RPage    -- deliver HTML col-, img-, blog page   text/html
             | RIcon    -- deliver JPG icon                     image/jpg
             | RImg     -- deliver JPG image                    image/jpg
             | RBlog    -- ???
             | RVideo   -- deliver mp4 video                    ???/mp4
             | RStatic  -- deliver static page                  text/css, ...
             | RNull    -- error case
               deriving (Eq, Ord, Show)

data Req' a
  = Req' { _rType    :: ReqType      -- type
         , _rPathPos :: PathPos      -- collection path and maybe index
         , _rGeo     :: Geo          -- size of image or screen
         , _rVal     :: a            -- varying data when processing request
         }

type Req'IdNode                a = Req'              (IdNode,  a)
type Req'IdNode'ImgRef         a = Req'IdNode        (ImgRef,  a)
type Req'IdNode'ImgRef'ImgType a = Req'IdNode'ImgRef (ImgType, a)

-- --------------------

deriving instance Show a => Show (Req' a)

instance IsEmpty (Req' a) where
  isempty r = r ^. rType == RNull

instance Semigroup (Req' a) where
  Req'{_rType = RNull} <> r2 = r2
  r1                  <> _  = r1

-- instance Semi (Req' a) where

emptyReq' :: Req' ()
emptyReq'
  = Req' { _rType    = RNull
         , _rPathPos = (mempty, mzero)
         , _rGeo     = geo'org
         , _rVal     = ()
         }

-- --------------------

rType :: Lens' (Req' a) ReqType
rType k r = (\ new -> r {_rType = new}) <$> k (_rType r)

rPathPos :: Lens' (Req' a) PathPos
rPathPos k r = (\ new -> r {_rPathPos = new}) <$> k (_rPathPos r)

rPath :: Lens' (Req' a) Path
rPath = rPathPos . _1

rPos :: Lens' (Req' a) Pos
rPos = rPathPos . _2

rGeo :: Lens' (Req' a) Geo
rGeo k r = (\ new -> r {_rGeo = new}) <$> k (_rGeo r)

rVal :: Lens (Req' a) (Req' b) a b
rVal k r = (\ new -> r {_rVal = new}) <$> k (_rVal r)

rIdNode :: Lens' (Req'IdNode a) IdNode
rIdNode = rVal . _1

rColId :: Lens' (Req'IdNode a) ObjId
rColId = rIdNode . _1

rColNode :: Lens' (Req'IdNode a) ImgNode
rColNode = rIdNode . _2

rImgRef :: Lens' (Req'IdNode'ImgRef a) ImgRef
rImgRef = rVal . _2 . _1

rImgType :: Lens' (Req'IdNode'ImgRef'ImgType a)  ImgType
rImgType = rVal . _2 . _2 . _1

-- --------------------

normAndSetIdNode :: Req' a -> Cmd (Req'IdNode a)
normAndSetIdNode = setIdNode >=> normPathPos

-- if a path ("abx/def", Just i) points to a collection "ghi"
-- the path is normalized to ("abc/def/ghi", Nothing)

normPathPos :: Req'IdNode a -> Cmd (Req'IdNode a)
normPathPos r =
  case r ^. rPos of
    Just pos
      -> do ce <- colEntryAt pos (r ^. rColNode)
            colEntry'
              (const $ return r)
              (\ c' ->
                  do p' <- objid2path c'
                     setIdNode (r & rVal     %~ snd
                                  & rPathPos .~ (p', mzero)
                               )
              )
              ce
    Nothing
      -> return r

-- check the existence of a path
-- and add (objid, imgnode) to the request

setIdNode :: Req' a -> Cmd (Req'IdNode a)
setIdNode r = do
  (i, n) <- getIdNode' (r ^. rPath)
  unless (isCOL n) $
    abortR "processHtmlPage: path not found: " r

  return (r & rVal %~ ((i, n), ))

-- check whether an image ref is legal
-- and add the image ref to the result
-- in case of a ref to a collection, the empty ImgRef is set

setImgRef :: Req'IdNode a -> Cmd (Req'IdNode'ImgRef a)
setImgRef = setRef . toImgRef
  where
    setRef r =
      case r ^. rPos of
        Just pos
          -> do ce <- colEntryAt pos (r ^. rColNode)
                colEntry'
                  (\ ir -> return (r & rImgRef .~ ir))
                  (const $ return r')
                  ce

        Nothing
          -> return r'
      where
        r' =  r & rType .~ RNull

-- add the col image ref or col blog ref to the request

setColImgRef :: Req'IdNode a -> Cmd (Req'IdNode'ImgRef a)
setColImgRef = setColRef' theColImg

setColBlogRef :: Req'IdNode a -> Cmd (Req'IdNode'ImgRef a)
setColBlogRef = setColRef' theColBlog

setColRef' :: Traversal' ImgNode (Maybe ImgRef)
              -> Req'IdNode a
              -> Cmd (Req'IdNode'ImgRef a)
setColRef' theC = return . setRef . toImgRef
  where
    setRef r
      | Just _pos <- r ^. rPos                       = r'
      | Just ir   <- r ^? rColNode . theC . traverse = r & rImgRef .~ ir
      | otherwise                                    = r'
      where
        r' =  r' & rType .~ RNull

toImgRef :: Req'IdNode a -> Req'IdNode'ImgRef a
toImgRef r = r & rVal . _2 %~ (emptyImgRef, )


-- --------------------
--
-- really needed? Type of an ImgRef can be computed
-- by looking at the extension of the name
--
-- compute the type of the image/blog/video/...

setImgType :: Req'IdNode'ImgRef a
           -> Cmd (Req'IdNode'ImgRef'ImgType a)
setImgType = setType . toImgType
  where
    setType =
      processReq'IdNode'ImgRef
      (\   r -> return (r & rType    .~ RNull))
      (\ p r -> return (r & rImgType .~ (p ^. theImgType)))

toImgType :: Req'IdNode'ImgRef a -> Req'IdNode'ImgRef'ImgType a
toImgType r = r & rVal . _2 . _2 %~ (IMGother, )


-- compute the request type from a media entry
-- needed when generating URL's for media files

imgType2ReqType :: Req'IdNode'ImgRef'ImgType a -> Req'IdNode'ImgRef'ImgType a
imgType2ReqType r = r & rType .~ rt (r ^. rImgType)
  where
    rt IMGcopy  = RImg
    rt IMGimg   = RImg
    rt IMGjpg   = RImg
    rt IMGtxt   = RBlog
    rt IMGvideo = RVideo
    rt _        = RNull

-- --------------------
--
-- abort processing of a request

abortR :: String -> (Req' a) -> Cmd b
abortR msg r =
  abort (msg ++ ": req = " ++ toUrlPath r)

-- --------------------
--
-- build file paths from a request

-- the "raw" path of a request
-- a static file path or a collection path,
-- maybe with index into the col
-- prefixed by a geometry spec
--
-- example: /160x120/archive/pictures/abc/pic-0001

toRawPath :: Req' a -> FilePath
toRawPath r =
  case r ^. rType of
    RNull   -> mempty
    RStatic -> path'
    _       -> "/" ++ geo' ++ path' ++ pos'
  where
    path' = r ^. rPath . isoString
    geo'  = r ^. rGeo  . isoString
    pos'  = maybe mempty (\ i -> "/" ++ toS i) $ r ^. rPos

    toS =
      ("pic-" ++ ) . reverse . take 4 . reverse . ("0000" ++ ). show

-- --------------------
--
-- build the raw path and add appropriate extension
-- and prefix with request type
--
-- example: /icon/160x120/archive/pictures/abc/pic-0001.jpg
--          /page/1920x1200/archive/pictures/abc.html

toUrlPath :: Req' a -> FilePath
toUrlPath r =
  case ty of
    RNull   -> mempty
    RStatic -> fp
    _       -> px ++ fp ++ ex ty
  where
    ty = r ^. rType
    fp = toRawPath r
    px = "/" ++ (map toLower $ show ty)

    ex RPage  = ".html"
    ex RIcon  = ".jpg"
    ex RImg   = ".jpg"
    ex RBlog  = ".html"
    ex RVideo = ".mp4"
    ex _      = ""

-- --------------------
--
-- build the source path for an image file
-- from a col entry an an ImgRef
-- used for converting the image file into
-- a file required by the request url
--
-- example usage: genImage    r (toSourcePath r) (toUrlPath r)
--                genBlogPage r (toSourcePath r) (toUrlPath r)

toSourcePath :: Req'IdNode'ImgRef a -> Cmd FilePath
toSourcePath =
  processReq'IdNode'ImgRef
  (const $ return mempty)
  srcPath
  where
    srcPath _part r = do
      p' <- substPathName nm <$> objid2path oid
      toFilePath p'
      where
        ImgRef oid nm = r ^. rImgRef

-- --------------------
--
-- check the media type of an ImgRef
--
-- example: .jpg file:      checkMedia mediaIsJpg r
--          other img file: checkMedia mediaIsImg r
--
-- the media check can be done without looking at the ImgType of the
-- appropriate image part, so the part could be removed (???)

checkMedia :: (String -> Bool)
           -> Req'IdNode'ImgRef a -> Bool
checkMedia checkExt r =
  not (isempty r)
  &&
  checkExt (r ^. rImgRef . to _iname . isoString)

-- --------------------
--
-- process a request with image ref
-- result depends on the image part given in the ref
--
-- pNull processes null req
-- pNode processes the image part
--
-- used in toSourcePath and setImgType

processReq'IdNode'ImgRef :: (           Req'IdNode'ImgRef a -> Cmd b)
                         -> (ImgPart -> Req'IdNode'ImgRef a -> Cmd b)
                         -> (           Req'IdNode'ImgRef a -> Cmd b)
processReq'IdNode'ImgRef pNull pNode r
  | isempty r =
      pNull r
  | otherwise = do
      n <- getImgVal oid
      maybe
        (abortR ("image part not found " ++ show (nm ^. isoString)) r)
        (\ p -> pNode p r)
        (n ^? theImgPart nm)
  where
    ImgRef oid nm = r ^. rImgRef

-- --------------------

processReqIcon :: Req' a -> Cmd FilePath
processReqIcon r0 = do
  r1 <- normAndSetIdNode r0
  let dp = toUrlPath r1

  case r1 ^. rPos of
    -- create an icon from a media file
    Just _pos -> do
      r2 <- setImgRef r1
      sp <- toSourcePath r2
      genIcon r2 sp dp

    -- create an icon for a collection
    Nothing -> do
      r2 <- setColImgRef r1
      if isempty r2
        -- collection does not have a front page image
        then createIcon r1 dp
        else do
             sp <- toSourcePath r2
             genIcon r2 sp dp

  return dp

-- dispatch icon generation over media type (jpg, txt, md, video)
genIcon :: Req'IdNode'ImgRef a -> FilePath -> FilePath -> Cmd ()
genIcon r sp dp = return ()

-- create an icon from a piece of text, name, ...
createIcon :: Req'IdNode a -> FilePath -> Cmd ()
createIcon r dst = return ()

-- --------------------
{-
data Req
  = Req { _reqPath  :: Path
        , _reqIx    :: Maybe Int
        , _reqGeoAR :: GeoAR
        , _reqColId :: ObjId
        , _reqColNd :: ImgNode
        , _reqImgId :: ObjId
        , _reqImgNd :: ImgNode
        , _reqIName :: Name
        , _reqIType :: ImgType
        , _reqRes   :: LazyByteString
        }

deriving instance Show Req

emptyReq :: Req
emptyReq =
  Req { _reqPath  = mempty
      , _reqIx    = mzero
      , _reqGeoAR = geoar'org
      , _reqColId = mempty
      , _reqColNd = emptyImgCol
      , _reqImgId = mempty
      , _reqImgNd = emptyImg
      , _reqIName = mempty
      , _reqIType = mempty
      , _reqRes   = mempty
      }

-- --------------------

reqPath :: Lens' Req Path
reqPath k r = (\ new -> r {_reqPath = new}) <$> k (_reqPath r)

reqIx :: Lens' Req (Maybe Int)
reqIx k r = (\ new -> r {_reqIx = new}) <$> k (_reqIx r)

reqGeoAR :: Lens' Req GeoAR
reqGeoAR k r = (\ new -> r {_reqGeoAR = new}) <$> k (_reqGeoAR r)

reqColId :: Lens' Req ObjId
reqColId k r = (\ new -> r {_reqColId = new}) <$> k (_reqColId r)

reqColNd :: Lens' Req ImgNode
reqColNd k r = (\ new -> r {_reqColNd = new}) <$> k (_reqColNd r)

reqImgId :: Lens' Req ObjId
reqImgId k r = (\ new -> r {_reqImgId = new}) <$> k (_reqImgId r)

reqImgNd :: Lens' Req ImgNode
reqImgNd k r = (\ new -> r {_reqImgNd = new}) <$> k (_reqImgNd r)

reqIName :: Lens' Req Name
reqIName k r = (\ new -> r {_reqIName = new}) <$> k (_reqIName r)

reqIType :: Lens' Req ImgType
reqIType k r = (\ new -> r {_reqIType = new}) <$> k (_reqIType r)

reqRes :: Lens' Req LazyByteString
reqRes k r = (\ new -> r {_reqRes = new}) <$> k (_reqRes r)

-- ----------------------------------------
--
-- dispatch HTML page generation
-- depending on type of object (collection, image, blog entry)

processHtmlPage :: Req -> Cmd Req
processHtmlPage r = do
  (i, n) <- getIdNode' (r ^. reqPath)
  unless (isCOL n) $
    abortWF "processHtmlPage: path not found: " r

  -- set collection id and collection node
  let r' = r & reqColId .~ i
             & reqColNd .~ n

  case r ^. reqIx of
    Just ix' -> do
      ce <- colEntryAt ix' n
      colEntry
        (\ i' nm' ->
            -- set image id and image name
            -- and process image (or blog entry or ...)
            processHtmlObjPage (r' & reqImgId .~ i'
                                   & reqIName .~ nm'
                               )
        )
        (\ c' -> do
            -- normalize path to a collection (remove ix)
            -- and restart processing
            p' <- objid2path c'
            processHtmlPage    (r' & reqPath .~ p'
                                   & reqIx   .~ mzero
                               )
        )
        ce
    Nothing ->
      -- generate a collection page
      processHtmlColPage r'


processHtmlObjPage :: Req -> Cmd Req
processHtmlObjPage r = do
  n  <- getImgVal (r ^. reqImgId)

  let r' = r & reqImgNd .~ n

  -- compute image type
  ty <- maybe
        (abortWF "processHtmlObjPage: image part not found" r')
        return
        (n ^? theImgPart (r' ^. reqIName) . theImgType)

  -- and store type
  let r'' = r' & reqIType .~ ty

  -- dispatch over image type
  case ty of
    IMGtxt
        -> processHtmlBlogPage r''
    _
      | ty `elem` [IMGjpg, IMGimg, IMGcopy]
        -> processHtmlImgPage r''

      | otherwise
        -> abortWF "processHtmlObjPage: HTML for image type not supported" r''

processHtmlColPage :: Req -> Cmd Req
processHtmlColPage r = undefined

processHtmlImgPage :: Req -> Cmd Req
processHtmlImgPage r = undefined

processHtmlBlogPage :: Req -> Cmd Req
processHtmlBlogPage r = undefined

abortWF :: String -> Req -> Cmd a
abortWF msg r = abort (msg ++ ": req = " ++ show r)

-- ----------------------------------------
-}
