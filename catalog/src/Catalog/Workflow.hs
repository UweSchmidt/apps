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
import Catalog.FilePath       (fileName2ImgType)
import Catalog.Html.Basic     (baseNameParser, ymdParser)
import Catalog.System.Convert (createResizedImage, genIcon)
import Text.SimpleParser      (parseMaybe)

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
             | RRef     -- deliver an url, not a content
             deriving (Eq, Ord, Show, Read)

data Req' a
  = Req' { _rType    :: ReqType      -- type
         , _rPathPos :: PathPos      -- collection path and maybe index
         , _rGeo     :: Geo          -- size of image or screen
         , _rVal     :: a            -- varying data when processing request
         }

type Req'IdNode                a = Req'              (IdNode,  a)
type Req'IdNode'ImgRef         a = Req'IdNode        (ImgRef,  a)

-- --------------------

deriving instance Show a => Show (Req' a)

emptyReq' :: Req' ()
emptyReq' =
  Req' { _rType = RRef
       , _rPathPos = (mempty, Nothing)
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

instance IsoString ReqType where
  isoString = iso toS frS
    where
      toS = drop 1 . map toLower . show
      frS = fromMaybe RRef .
            readMaybe .
            (\ (x : xs) -> 'R' : toUpper x : xs) .
            (++ " ")

-- ----------------------------------------
--
-- commands working in MaybeT Cmd

normAndSetIdNode :: Req' a -> CmdMB (Req'IdNode a)
normAndSetIdNode = setIdNode >=> normPathPos


-- check the existence of a path
-- and add (objid, imgnode) to the request

setIdNode :: Req' a -> CmdMB (Req'IdNode a)
setIdNode r = do
  i'n <- lift $ getIdNode' (r ^. rPath)
  return (r & rVal %~ (i'n, ))


-- if a path ("/abx/def", Just i) points to a collection "ghi"
-- the path is normalized to ("/abc/def/ghi", Nothing)

normPathPos :: Req'IdNode a -> CmdMB (Req'IdNode a)
normPathPos r =
  ( do pos <- pureMB (r ^. rPos)
       ce  <- lift $ colEntryAt pos (r ^. rColNode)
       colEntry'
         (const $ return r)
         (normPathPosC r)
         ce
  )
  <|>
  return r

normPathPosC :: Req'IdNode a -> ObjId -> CmdMB (Req'IdNode a)
normPathPosC r c' =
  do p' <- lift $ objid2path c'
     setIdNode (r & rVal     %~ snd
                  & rPathPos .~ (p', mzero)
               )


-- compute a req with a path and pos
-- this is id when an image is requested
-- in case of a collection the parent col and the pos there are
-- computed
--
-- inverse of normPathPos

denormPathPos :: Req'IdNode a -> CmdMB (Req'IdNode a)
denormPathPos r =
  (pureMB (r ^. rPos) >>= (const $ return r))
  <|>
  ( do par'i <- lift   $ getImgParent (r ^.rColId)
       par'n <- lift   $ getImgVal    par'i
       par'p <- lift   $ objid2path   par'i
       par'x <- pureMB $ lookupOId    (r ^. rColId) par'n
       return
         (r & rPathPos .~ (par'p, Just par'x)
            & rIdNode  .~ (par'i, par'n)
         )
  )
  where
    lookupOId oid pnd =
      searchPos
      ((== oid) . (^. theColObjId))
      (pnd ^. theColEntries)

-- --------------------
--
-- check whether an image ref is legal
-- and add the image ref to the result
-- in case of a ref to a collection, the empty ImgRef is set

setImgRef :: Req'IdNode a -> CmdMB (Req'IdNode'ImgRef a)
setImgRef r = do
  pos <- pureMB (r ^. rPos)
  ce  <- lift $ colEntryAt pos (r ^. rColNode)
  colEntry'
      (\ ir -> return (r & rVal . _2 %~ (ir, )))
      (const mzero)
      ce

-- add the col image ref or col blog ref to the request

setColImgRef :: Req'IdNode a -> CmdMB (Req'IdNode'ImgRef a)
setColImgRef = setColRef' theColImg

setColBlogRef :: Req'IdNode a -> CmdMB (Req'IdNode'ImgRef a)
setColBlogRef = setColRef' theColBlog

setColRef' :: Traversal' ImgNode (Maybe ImgRef)
            -> Req'IdNode a
            -> CmdMB (Req'IdNode'ImgRef a)
setColRef' theC r =
  do ir <- pureMB $ r ^? rColNode . theC . traverse
     return (r & rVal . _2 %~ (ir, ))

-- --------------------
--
-- handle an img/icon request

processReqImg' :: Req' a -> CmdMB FilePath
processReqImg' r0 = do
  r1 <- normAndSetIdNode r0
  let dp = toUrlPath r1
  lift $ trc $ "processReqImg: " ++ show dp

  case r1 ^. rPos of
    -- create an icon from a media file
    Just _pos -> do
      r2 <- setImgRef  r1
      lift $ genReqImg r2 dp
      return dp

    -- create an icon for a collection
    Nothing ->
      ( do r2 <- setColImgRef  r1
                 <|>
                 setColBlogRef r1
           lift $ genReqImg r2 dp
           return dp
      )
      <|>
      ( do lift $ createIconFromObj r1 dp
           return dp
      )

-- ----------------------------------------
--
-- handle a html page request

processReqPage' :: Req' a -> CmdMB Blaze.Html
processReqPage' r0 = do
  r1 <- normAndSetIdNode r0
  let dp = toUrlPath r1
  lift $ trc $ "processReqPage: " ++ show dp

  case r1 ^. rPos of
    -- create an image page
    Just _pos -> do
      r2 <- setImgRef      r1
      lift $ genReqImgPage r2 dp

    -- create a collection page
    Nothing -> do
      lift $ genReqColPage r1 dp

-- ----------------------------------------
--
-- main entry points

processReqImg ::  Req' a -> Cmd FilePath
processReqImg = processReq processReqImg'

processReqPage :: Req' a -> Cmd Blaze.Html
processReqPage = processReq processReqPage'

processReq :: (Req' a -> CmdMB b) -> Req' a -> Cmd b
processReq cmd r0 =
  runMaybeT (cmd r0)
  >>=
  maybe (abortR "processReq: error in processing " r0) return

-- ----------------------------------------
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
  "/" ++ geo' ++ path' ++ pos'
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
  rt ++ px ++ fp ++ ex ty
  where
    ty = r ^. rType
    fp = toRawPath r
    px = "/" ++ ty ^. isoString
    rt = ps'docroot

    ex RPage  = ".html"
    ex RIcon  = ".jpg"
    ex RImg   = ".jpg"
    ex RBlog  = ".html"
    ex RVideo = ".mp4"
    ex _      = ""


-- --------------------
--
-- build the source path for an image file
-- from a col entry and an ImgRef
-- used for converting the image file into
-- a file required by the request url
--
-- example usage: genImage    r (toSourcePath r) (toUrlPath r)
--                genBlogPage r (toSourcePath r) (toUrlPath r)

toSourcePath :: Req'IdNode'ImgRef a -> Cmd FilePath
toSourcePath r = do
  p <- objid2path i
  return $ (tailPath $ substPathName nm p) ^. isoString
  where
    ImgRef i nm = r ^. rImgRef

toCachedImgPath :: Req'IdNode'ImgRef a -> Cmd FilePath
toCachedImgPath r =
  addP <$> toSourcePath r
  where
    addP fp =
      ps'docroot </>
      r ^. rType . isoString </>
      r ^. rGeo  . isoString ++
      fp ++ ".jpg"

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
  checkExt (r ^. rImgRef . to _iname . isoString)


-- ----------------------------------------
--
-- commands for icon and image generation

-- dispatch icon generation over media type (jpg, txt, md, video)

genReqImg :: Req'IdNode'ImgRef a -> FilePath -> Cmd ()
genReqImg r dp = do
  sp <- toSourcePath r
  trc $ "genReqIcon sp=" ++ show sp ++ ", dp=" ++ show dp

  ip <- toCachedImgPath r
  case ity of
    IMGjpg ->
      createCopyFromImg geo sp ip dp

    IMGimg ->
      createCopyFromImg geo sp ip dp

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
    geo = mkGeoAR (r ^. rGeo) (reqType2AR $ r ^. rType)
    ity = fileName2ImgType (r ^. rImgRef . to _iname . isoString)


reqType2AR :: ReqType -> AspectRatio
reqType2AR RIcon = Fix
reqType2AR RImg  = Pad
reqType2AR _     = Pad -- default

-- read text from a file (blog entry) to generate an icon

getTxtFromFile :: FilePath -> Cmd String
getTxtFromFile sp = do
  txt <- cut 32 . T.concat . take 1 .
         filter (not . T.null) . map cleanup . T.lines <$>
         (toSysPath sp >>= readFileT')
  return (txt ^. isoString)
  where
    cleanup :: Text -> Text
    cleanup = T.dropWhile (not . isAlphaNum)

    cut :: Int -> Text -> Text
    cut l t
      | T.length t <= l = t
      | otherwise       = T.take (l - 3) t <> "..."


-- create an icon from the title of the path of a collection

createIconFromObj :: Req'IdNode a -> FilePath -> Cmd ()
createIconFromObj r dp = do
  trc $ "createIconFromObj: " ++ dp

  -- read the collection title
  str1 <- getImgVals
          (r ^. rColId)
          (theMetaData . metaDataAt descrTitle . isoString)
  trc $ "createIconFromObj: " ++ str1

  str2 <- if isempty str1
          -- if no title there, extract text from path
          then do
               p <- objid2path (r ^. rColId)
               return $ path2str (p ^. isoString)
          else return str1
  trc $ "createIconFromObj: " ++ str2

  createIconFromString geo str2 dp
    where
      geo = mkGeoAR (r ^. rGeo) (reqType2AR $ r ^. rType)

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

-- --------------------
--
-- create an image copy with a given geometry
-- from a source image sp0
--
-- the copy is stored under the path of the image, not the col entry
-- and then a link to this file is created with the col entry path
-- with this 2 step process all copies in all collection of the same
-- image share the same physical image file

createCopyFromImg :: GeoAR -> FilePath -> FilePath -> FilePath -> Cmd ()
createCopyFromImg geo sp0 ip dp0 =
  withCache resizeAndLink sp0 dp0
  where
    resizeAndLink sp dp = do
      withCache (createResizedImage geo) sp ip
      withCache linkCopy                 ip dp
        where
          linkCopy s d = do
            s' <- toSysPath s
            d' <- toSysPath d
            linkFile s' d'

-- --------------------

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
      fn = ps'gen'icon </> str2fn str ++ ".jpg"
      str2fn = concatMap urlEnc
        where
          urlEnc c
            | isAlphaNum c = [c]
            | otherwise    = "-" ++ show (fromEnum c) ++ "-"

-- ----------------------------------------
--
-- html page generation

genReqImgPage :: Req'IdNode'ImgRef a -> FilePath -> Cmd Blaze.Html
genReqImgPage r dp = do
  abortR ("genReqImgPage: TODO") r

genReqColPage :: Req'IdNode a -> FilePath -> Cmd Blaze.Html
genReqColPage r dp = do
  abortR ("genReqColPage: TODO") r

-- --------------------
--
-- cache generated files, e.g. icons, scaled down images, ...
--
-- the cached entry gets the same time stamp as the source
-- cache hit only when both time stamps are the same

withCache :: (FilePath -> FilePath -> Cmd ())
          ->  FilePath -> FilePath -> Cmd ()
withCache cmd sp dp = do
  sp' <- toSysPath sp
  dp' <- toSysPath dp
  sw  <- getModiTime' sp'
  dw  <- getModiTime' dp'
  trc $ "withCache: " ++ show ((sp, sw), (dp, dw))

  unless (dw == sw && not (isempty dw)) $ do
    -- no cache hit
    -- execute command and
    -- set mtime of dest to mtime of source
    -- so cache hits are those with equal mtime timestamp (dw == ws)
    trc "withCache: cache miss"
    createDir (takeDirectory <$> dp')
    cmd sp dp
    setModiTime sw dp'

-- ----------------------------------------
--
-- abort processing of a request

abortR :: String -> (Req' a) -> Cmd b
abortR msg r =
  abort (msg ++ ": req = " ++ toUrlPath r)


-- --------------------
--
-- navigation ops

toParent :: Req'IdNode a -> CmdMB (Req'IdNode a)
toParent r = do
  p <- denormPathPos r
  return (p & rPos .~ Nothing)

toPos' :: (Int -> Int) -> Req'IdNode a -> CmdMB (Req'IdNode a)
toPos' f r = do
  p <- denormPathPos r
  x <- pureMB (p ^. rPos)
  let x' = f x
  _ <- pureMB (p ^? rColNode . theColEntries . ix x')
  normPathPos (p & rPos .~ Just x')

toPrev :: Req'IdNode a -> CmdMB (Req'IdNode a)
toPrev = toPos' pred

toNext :: Req'IdNode a -> CmdMB (Req'IdNode a)
toNext = toPos' succ

toFirst :: Req'IdNode a -> CmdMB (Req'IdNode a)
toFirst = toPos' (const 0)

-- normalization must be done before
toChildren :: Req'IdNode a -> CmdMB [Req'IdNode a]
toChildren r =
  mapM normC $ zip [0..] (r ^. rColNode . theColEntries)
  where
    normC (i, ce) =
      colEntry'
        (const $ return (r & rPos .~ Just i))
        (normPathPosC r)
        ce

-- ----------------------------------------
