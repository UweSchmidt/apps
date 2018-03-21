{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Catalog.Workflow
where

import Data.Prim
import Data.ImgNode
import Data.ImgTree
import Data.MetaData
import Catalog.Cmd
import Catalog.FilePath -- (fileName2ImgType, )
import Catalog.Html.Basic (baseNameParser, ymdParser)
import Catalog.System.Convert (createResizedImage, genIcon)
import Text.SimpleParser (parseMaybe)

import qualified Data.Text            as T
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
      genReqIcon r2 sp dp

    -- create an icon for a collection
    Nothing -> do
      r2 <- setColImgRef r1
      if isempty r2
        -- col does not have a front page image
        then do r2' <- setColBlogRef r1
                if isempty r2'
                  -- no col image, not col blog
                  -- create an icon from col title or name
                  then createIconFromObj r1 dp
                  -- col has a blog text (with headline)
                  else  do sp <- toSourcePath r2'
                           genReqIcon r2' sp dp
        -- col has a front page image
        else do sp <- toSourcePath r2
                genReqIcon r2 sp dp

  return dp

-- dispatch icon generation over media type (jpg, txt, md, video)
genReqIcon :: Req'IdNode'ImgRef a -> FilePath -> FilePath -> Cmd ()
genReqIcon r sp dp =
  case ity of
    IMGjpg ->
      createIconFromImg geo sp dp

    IMGimg ->
      createIconFromImg geo sp dp

    IMGvideo ->
      abortR "genReqIcon: icon for video not yet implemented" r

    IMGtxt -> do
      -- read text from source file
      -- if no text there,
      -- fall back to create icon from object path

      str <- getTxtFromFile sp
      if isempty str
        then createIconFromObj    r       dp
        else createIconFromString geo str dp

    _ ->
      abortR "genReqIcon: no icon for image type" r
  where
    geo = mkGeoAR (r ^. rGeo) Fix
    ity = fileName2ImgType (r ^. rImgRef . to _iname . isoString)


-- create an icon from the title of the path of a collection

createIconFromObj :: Req'IdNode a -> FilePath -> Cmd ()
createIconFromObj r dp = do
  -- read the collection title
  str1 <- getImgVals
          (r ^. rColId)
          (theMetaData . metaDataAt descrTitle . isoString)

  str2 <- if isempty str1
          -- if no title there, extract text from path
          then do
               p <- objid2path (r ^. rColId)
               return $ path2str (p ^. isoString)
          else return str1

  createIconFromString geo str2 dp
    where
      geo = mkGeoAR (r ^. rGeo) Fix

      path2str :: String -> String
      path2str f
        | Just (y, Nothing)           <- ymd = y
        | Just (y, Just (m, Nothing)) <- ymd = toN m ++ "." ++ y
        | Just (y, Just (m, Just d))  <- ymd = toN d ++ "." ++ toN m ++ "." ++ y
        | Just n <- nm                       = n
        | otherwise                          = "?"
        where
          ymd = parseMaybe ymdParser      f
          nm  = parseMaybe baseNameParser f

          toN :: String -> String
          toN s = show i   -- remove leading 0's
            where
              i :: Int
              i = read s

createIconFromImg :: GeoAR -> FilePath -> FilePath -> Cmd ()
createIconFromImg geo sp dp =
  withCache (createResizedImage geo) sp dp

createIconFromString :: GeoAR -> String -> FilePath -> Cmd ()
createIconFromString geo str dp = do
  sp <- createRawIconFromString str
  withCache (createResizedImage geo) sp dp

createRawIconFromString :: String -> Cmd FilePath
createRawIconFromString str = do
  trc $ "createRawIconFromString: " ++ show fn ++ " " ++ show str
  genIcon fn str
  return fn
    where
      fn = ps'iconsgen </> str2fn str ++ ".jpg"
      str2fn = concatMap urlEnc
        where
          urlEnc c
            | isAlphaNum c = [c]
            | otherwise    = "-" ++ show (fromEnum c) ++ "-"

getTxtFromFile :: FilePath -> Cmd String
getTxtFromFile sp = do
  txt <- cut 32 . T.concat . take 1 .
         filter (not . T.null) . map cleanup . T.lines <$>
         readFileT' sp
  return (txt ^. isoString)
  where
    cleanup :: Text -> Text
    cleanup = T.dropWhile (not . isAlphaNum)

    cut :: Int -> Text -> Text
    cut l t
      | T.length t <= l = t
      | otherwise       = T.take (l - 3) t <> "..."

-- --------------------
--
-- cache generated files, e.g. icons, scaled down images, ...

withCache :: (FilePath -> FilePath -> Cmd ())
          ->  FilePath -> FilePath -> Cmd ()
withCache cmd sp dp = do
  sw <- getModiTime' sp
  dw <- getModiTime' dp
  trc $ "withCache: " ++ show ((sp, sw), (dp, dw))

  unless (dw == sw && not (isempty dw)) $ do
    -- no cache hit
    -- execute command and
    -- set mtime of dest to mtime of source
    -- so cache hits are those with equal mtime timestamp (dw == ws)
    trc "withCache: cache miss"
    cmd sp dp
    setModiTime sw dp

-- --------------------
