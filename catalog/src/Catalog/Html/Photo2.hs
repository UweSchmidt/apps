{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Catalog.Html.Photo2
where

import Catalog.Cmd
-- import Data.ImageStore
import Data.ImgTree
import Data.MetaData
import Data.Prim

import Catalog.Html.Templates.Photo2.AlbumPage (photo2Tmpl)
import Catalog.Journal
import Catalog.System.Convert ( genAssetIcon
                              , genBlogText
                              , genBlogHtml
                              , writeBlogText
                              )
import Catalog.System.ExifTool (getMetaData)

import Text.SimpleTemplate

-- ----------------------------------------

type Html = [Text]

-- a page config has an name, a geo for the pictures,
-- a geo for the icons,
-- and the # icons per row

type PageConfig = (String, (GeoAR, GeoAR, Int, TmplEnv Cmd))

-- an entry in a collection is identified by an ObjId, Path, Filepath, ...
-- and, an index into the list of collection entries, or it's a reference
-- to the collection itself

type ColRef' a   = (a, Maybe Int)
type ColRef      = ColRef' ObjId

type ActCmd      = TmplAct Cmd

-- ----------------------------------------

thePageConfigs :: [PageConfig]
thePageConfigs =
  [ ("html-1600x1200", ( GeoAR 1600 1200 Pad
                       , GeoAR   160 120 Fix
                       , 9
                       , photo2Tmpl
                       )
    )
  , ("html-1400x1050", ( GeoAR 1400 1050 Pad
                       , GeoAR  140  105 Fix
                       , 9
                       , photo2Tmpl
                       )
    )
  , ("html-1280x800",  ( GeoAR 1280  800 Pad
                       , GeoAR  120   90 Fix
                       , 9
                       , photo2Tmpl
                       )
    )
  ]

-- ----------------------------------------

pagePathExpr :: Regex
pagePathExpr =
  parseRegexExt $
  "/({config}[a-z]+-[0-9]+x[0-9]+)" ++
  "(" ++
  "(({path}/archive/collections/.*)(/pic-({no}[0-9]+))[.]html)" ++
  "{|}" ++
  "(({path}/archive/collections(/.*)?)[.]html)" ++
  ")"


isoPicNo :: Iso' Int String
isoPicNo = iso toS frS
  where
    toS =
      ("pic-" ++ ) . reverse . take 4 . reverse . ("0000" ++ ). show
    frS s =
      case matchSubex "pic-({no}[0-9]+)" s of
        [("no", no)] ->
          read no
        _ -> -1

-- ----------------------------------------

-- parse a .html url

url2confPathNo :: FilePath -> Cmd (String, ColRef' Path)
url2confPathNo f =
  case matchSubexRE pagePathExpr f of
    [("config", config), ("path", path), ("no", no)] ->
      return ( config
             , ( path ^. from isoString
               , Just $ read no
               )
             )
    [("config", config), ("path", path)] ->
      return ( config
             , ( path ^. from isoString
               , Nothing
               )
             )
    _ -> abort $ "can't process document ref " ++ show f

-- TODO : remove it
url2pathNo :: FilePath -> Cmd (ColRef' Path)
url2pathNo f = snd <$> url2confPathNo f

-- ----------------------------------------

url2confObjId :: FilePath -> Cmd (String, ColRef)
url2confObjId f = do
  (c, p) <- url2confPathNo f
  mi     <- crPath2crObjId p
  i      <- fromJustCmd ("no entry found for href " ++ show f) mi
  return (c, i)

url2objId :: FilePath -> Cmd ColRef
url2objId f = snd <$> url2confObjId f

-- convert a path ref into an object ref
crPath2crObjId :: ColRef' Path -> Cmd (Maybe ColRef)
crPath2crObjId (p, cix) = do
  i <- fst <$> getIdNode' p
  normColRef (i, cix)

-- ----------------------------------------

maybeColRef :: (ObjId -> a) -> (ObjId -> Int -> a) -> ColRef -> a
maybeColRef cref iref (i, p) =
  case p of
    Nothing  -> cref i
    Just pos -> iref i pos

mkColRefC :: ObjId -> ColRef
mkColRefC i = (i, Nothing)

mkColRefI :: ObjId -> Int -> ColRef
mkColRefI i pos = (i, Just pos)

-- ----------------------------------------

-- normalize a colref
-- if the result sub index is Nothing, the ref points to a collection
-- else the ref points to an image

normColRef :: ColRef -> Cmd (Maybe ColRef)
normColRef = maybeColRef cref iref
  where
    cref i = return $ Just $ mkColRefC i
    iref i pos = do
      val <- getImgVal i
      return $
        case val ^? theColEntries . ix pos of
          Just (ImgRef _ _) ->    -- ref to an image
            Just $ mkColRefI i pos
          Just (ColRef j) ->      -- ref to a sub collection
            Just $ mkColRefC j
          Nothing ->              -- index out of bounds
            Nothing

-- ----------------------------------------
--
-- navigation in the collection tree
--
-- 1 step up, 1 step down (indexed),
-- and to the left or right by an offset

parentColRef :: ColRef -> Cmd (Maybe ColRef)
parentColRef = maybeColRef cref iref
  where
    cref i = do
      parent'i <- getImgParent i
      iscol    <- isCOL <$> getImgVal parent'i
      return $
        if iscol
        then Just $ mkColRefC parent'i
        else Nothing
    iref i _pos = return $ Just $ mkColRefC i

childColRef :: Int -> ColRef -> Cmd (Maybe ColRef)
childColRef pos = maybeColRef cref iref
  where
    cref i = do
      cs <- getImgVals i theColEntries
      case cs ^? ix pos of
        Just _ ->
          normColRef $ mkColRefI i pos
        Nothing ->  -- index out of bounds
          return Nothing
    iref _i _pos = return Nothing -- img ref


ixColRef :: ColRef -> Cmd (Maybe Int)
ixColRef = maybeColRef cref iref
  where
    cref i = do
      mp'i <- parentColRef $ mkColRefC i
      case mp'i of
        Nothing ->
          return $ Nothing
        Just (j, _) -> do
          cs <- getImgVals j theColEntries
          return $
            searchPos ((== i) . (^. theColObjId)) cs
    iref _i pos = return $ Just pos


neighborColRef :: Int -> ColRef -> Cmd (Maybe ColRef)
neighborColRef offset = maybeColRef cref iref
  where
    cref i = do
      let cr = mkColRefC i
      -- position in parent col
      mpos <- ixColRef cr
      case mpos of
        Nothing ->
          return Nothing
        Just pos -> do
          x <- parentColRef cr       -- 1 level up
          case x of
            Just parent'cr ->
              childColRef (pos + offset) parent'cr -- 1 level down
            _ ->
              return Nothing

    iref i pos = normColRef $ mkColRefI i (pos + offset)

prevColRef, nextColRef :: ColRef -> Cmd (Maybe ColRef)
prevColRef = neighborColRef (-1)
nextColRef = neighborColRef   1

-- ----------------------------------------

-- the main entry for a HTML page

genHtmlPage :: FilePath -> Cmd Text
genHtmlPage p =
  mconcat <$> genHtmlPage' p

genHtmlPage' :: FilePath -> Cmd Html
genHtmlPage' p = do
  ( pageConf,
    this'cr@(this'i, pos)) <- url2confObjId p

  (geo1, geo2, no'rows, env) <- fromJustCmd
    ("can't find config for " ++ show pageConf)
    (lookup pageConf thePageConfigs)


  -- pnp@(config, this'ref@(this'path, mno)) <- url2confPathNo p

  -- this entry
  this'type   <- colImgType               this'cr
  this'img    <- colImgPath               this'cr
  this'fname  <- ((^. from isoMaybe) . fmap (^. isoString)) <$>
                 colImgName               this'cr

  -- for a col: get the entries, for an img: empty
  this'cs     <- getImgVals this'i theColEntries
  this'meta   <- colImgMeta               this'cr
  this'ix     <- maybe "" (^. isoPicNo)
                             <$> ixColRef this'cr
  this'blog   <- colBlogCont this'type    this'cr
  this'cblog  <- colImgBlog               this'cr

  parent'cr   <- parentColRef             this'cr
  parent'href <- colHref pageConf   `bmb` parent'cr
  parent'img  <- colImgPath         `bmb` parent'cr
  parent'meta <- colImgMeta0        `bmb` parent'cr

  prev'cr     <- prevColRef               this'cr
  prev'href   <- colHref pageConf   `bmb` prev'cr
  prev'img    <- colImgPath         `bmb` prev'cr
  prev'meta   <- colImgMeta0        `bmb` prev'cr
  prev'type   <- colImgType         `bmb` prev'cr

  next'cr     <- nextColRef               this'cr
  next'href   <- colHref pageConf   `bmb` next'cr
  next'img    <- colImgPath         `bmb` next'cr
  next'meta   <- colImgMeta0        `bmb` next'cr
  next'type   <- colImgType         `bmb` next'cr

  child1'cr   <- childColRef 0            this'cr
  child1'href <- colHref pageConf   `bmb` child1'cr
  child1'img  <- colImgPath         `bmb` child1'cr
  child1'meta <- colImgMeta0        `bmb` child1'cr

  let noTxtRef ty ref
        | ty == IMGtxt = mempty
        | otherwise    = ref
  let next'href'    = noTxtRef next'type next'href
  let prev'href'    = noTxtRef prev'type prev'href

  let getMD md n    = md ^. metaDataAt n . isoString
  let getTitle md   = getMD md descrTitle

  let this'title    = take1st [ getTitle this'meta
                              , this'fname
                              ]
  let this'subtitle = getMD this'meta descrSubtitle
  let this'comment  = getMD this'meta descrComment
  let this'duration = take1st [ getMD this'meta descrDuration
                              , "1.0"
                              ]

  let parent'title  = getTitle parent'meta
  let prev'title    = getTitle prev'meta
  let next'title    = getTitle next'meta
  let child1'title  = getTitle child1'meta

  children'crs    <- mapM (flip childColRef this'cr) $
                     [0 .. length this'cs - 1]
  children'hrefs  <- mapM (colHref pageConf `bmb`) children'crs
  children'imgs   <- mapM (colImgPath       `bmb`) children'crs
  children'meta   <- mapM (colImgMeta0      `bmb`) children'crs

  let children'titles = map getTitle children'meta
  let children'5      = zip5
                        children'crs
                        children'hrefs
                        children'imgs
                        children'titles
                        [(0::Int)..]

  let addColon x      = if isempty x then x else ": "++ x

  let env' =
        env
        & addDefaultAct
        & insAct "rootPath"      (return "") -- (liftTA (use theMountPath) >>= xtxt)
        & insAct "theUpPath"     (return "")
        & insAct "theDate"       (liftTA (show <$> atThisMoment) >>= xtxt)
        & insAct "theTitle"      (xtxt this'title)
        & insAct "theSubTitle"   (xtxt this'subtitle)
        & insAct "theComment"    (xtxt this'comment)
        & insAct "theHeadTitle"  (xtxt this'title)
        & insAct "theDuration"   (xtxt this'duration)

        -- the img geo
        & insAct "theImgGeoDir"  (xtxt $ geo1 ^. isoString)
        & insAct "theIconGeoDir" (xtxt $ geo2 ^. isoString)
        & insAct "theImgGeo"     (xtxt $ geo1 ^. theGeo . isoString)
        & insAct "theIconGeo"    (xtxt $ geo2 ^. theGeo . isoString)

        -- the href's
        & insAct "thisHref"      (xtxt p)
        & insAct "thisPos"       (xtxt $ "#" ++ this'ix)
        & insAct "thePrevHref"   (xtxt prev'href)
        & insAct "theNextHref"   (xtxt next'href)
        & insAct "theParentHref" (xtxt parent'href)
        & insAct "theChild1Href" (xtxt child1'href)

        -- the titles
        & insAct "theParentTitle" (xtxt $ addColon parent'title)
        & insAct "theChild1Title" (xtxt $ addColon child1'title)
        & insAct "thePrevTitle"   (xtxt $ addColon prev'title)
        & insAct "theNextTitle"   (xtxt $ addColon next'title)

        -- the img hrefs
        & insAct "thePrevImgRef"    (applyNotNull prev'href')
        & insAct "theNextImgRef"    (applyNotNull next'href')
        & insAct "theChild1ImgRef"  (applyNotNull child1'href)
        & insAct "thisImgRef"       (blankIcon (Just this'cr) this'img)
        & insAct "parentImgRef"     (blankIcon parent'cr parent'img)
        & insAct "prevImgRef"       (blankIcon prev'cr   prev'img)
        & insAct "nextImgRef"       (blankIcon next'cr   next'img)
        & insAct "child1ImgRef"     (blankIcon child1'cr child1'img)
        -- always take the blank image for the colImg
        -- & insAct "colImg"           (applyNotNull this'img)

        -- the nav templates
        & insAct "parentNav"     (applyNotNull parent'href)
        & insAct "prevNav"       (applyNotNull prev'href)
        & insAct "nextNav"       (applyNotNull next'href)
        & insAct "child1Nav"     (applyNotNull child1'href)

        -- the blog section
        -- only inserted, when a blog doc is there
        & insAct "colBlog"       (applyNotNull this'cblog)  -- a txt in a collection page
        & insAct "cBlogContents" (ttxt this'cblog)          --    ""
        & insAct "blogContents"  (ttxt this'blog)           -- a txt "img" page

        -- the contents, a nested loop over all children
        -- 4 infos are available per image, the "href"" for the image page,
        -- the the "src" of the image, the title, and the position in the collection

        & insAct "colContents"   (applyNotNull children'5) -- content only inserted when there are any entries
        & insAct "colRows"
          ( applySeqs
            [ ( "colIcons"
              , \ row ->
                applySeqs               -- generate a single row
                [ ( "theChildHref"
                  , \ x -> xtxt $ x ^. _2
                  )
                , ( "theChildImgRef"
                  , \ x -> blankIcon (x ^. _1) (x ^. _3)
                  )
                , ( "theChildTitle"
                  , \ x -> xtxt $
                           let s = x ^. _4
                               i = x ^. _5
                           in if isempty s
                              then show (i + 1) ++ ". Bild"
                              else s
                  )
                , ( "theChildId"
                  , \ x -> xtxt $ x ^. _5 . isoPicNo
                  )
                ]
                row
              )
            ]
            (divideAt no'rows children'5) -- divide entries into rows
          )

        -- the meta data templates
        & insMetaData this'meta
        & insAct "fileRefJpg"    (applyNotNull this'fname)
        & insAct "fileRefJpgVal" (xtxt this'fname)

  -- to gen an image page the template name must be exchanged

  let tmpl
        | isNothing pos       = "colPage"
        | this'type == IMGtxt = "txtPage"
        | otherwise           = "picPage"

  res <- runTmplAct applyTmpl tmpl env'
  -- io $ putStrLn (mconcat res ^. isoString) -- readable test output
  return res

-- ----------------------------------------

insMetaData :: MetaData -> TmplEnv Cmd -> TmplEnv Cmd
insMetaData md env =
  env
  & insMD "descrTitle"                   (gmd descrTitle)
  & insMD "descrSubtitle"                (gmd descrSubtitle)
  & insMD "descrTitleEnglish"            (gmd descrTitleEnglish)
  & insMD "descrTitleLatin"              (gmd descrTitleLatin)
  & insMD "descrComment"                 (gmd descrComment)
  & insMD "descrWeb"                     (gmd descrWeb)
  & insMD "descrWikipedia"               (gmd descrWikipedia)
  & insMDmaps "descrGoogleMaps"          (md ^. metaDataAt "Composite:GPSPosition")

  & insMD "exifCreateDate"               (gmd "EXIF:CreateDate")
  & insMD "camCameraModelName"           (gmd "EXIF:Model")
  & insMD "camLensID"                    (gmd "Composite:LensID")
  & insMD "camLens"                      (gmd "Composite:LensSpec")
  & insMD "exifExposureTime"             (gmd "EXIF:ExposureTime")
  & insMD "exifExposureMode"             (gmd "EXIF:ExposureMode")
  & insMD "exifExposureProgram"          (gmd "EXIF:ExposureProgram")
  & insMD "exifExposureCompensation"     (gmd "EXIF:ExposureCompensation")
  & insMD "exifFNumber"                  (gmd "EXIF:FNumber")
  & insMD "exifFocusDistance"            (gmd "EXIF:FocusDistance")
  & insMD "exifDepthOfField"             (gmd "Composite:DOF")
  & insMD "exifISO"                      (gmd "EXIF:ISO")
  & insMD "exifFocalLength"              (gmd "EXIF:FocalLength")
  & insMD "exifFocalLengthIn35mmFormat"  (gmd "EXIF:FocalLengthIn35mmFormat")
  & insMD "exifShootingMode"             (gmd "EXIF:ShootingMode")
  & insMD "exifWhiteBalance"             (gmd "EXIF:WhiteBalance")
  & insMD "exifImageSize"                (gmd "Composite:ImageSize")
  & insMD "fileFileModificationDateTime" (gmd "File:FileModifyDate")
  & insMD "fileRefRaw"                   (gmd "File:Directory"
                                          <> "/" <>
                                          gmd "File:FileName"
                                         )
  & insMD "geoGPSLatitude"               (gmd "XMP:GPSLatitude")
  & insMD "geoGPSLongitude"              (gmd "XMP:GPSLongitude")
  -- altitude disabled, XMP altitude is nonsense
  & insMD "geoGPSAltitude"               (gmd "???:GPSAltitude")
  & insMD "geoGPSPosition"               (gmd "Composite:GPSPosition")

  where
    insMD :: Text -> Text -> TmplEnv Cmd -> TmplEnv Cmd
    insMD name res env'
      | isempty res  = env' & insAct name mzero
      | otherwise    = env' & insAct (name <> "Val") (return res)

    gmd :: Name -> Text
    gmd name = md ^. metaDataAt name . isoString . to escHTML . isoText

    insMDmaps :: Text -> Text -> TmplEnv Cmd -> TmplEnv Cmd
    insMDmaps name res' env' =
      env'
      & insMD name (res ^. to escHTML . isoText)
      & insAct "locGoogleMaps" (xtxt $ res ^. to loc2googleMapsUrl . from isoMaybe)
      where
        res = res' ^. isoString

-- ----------------------------------------

-- a little helpler for avoiding MaybeT transformer monad

bmb :: (Monad m, Monoid b) => (a -> m b) -> Maybe a -> m b
bmb cmd = maybe (return mempty) cmd

{-}
cmd :: ColRef -> Cmd (Maybe a)
cr  ::  Maybe ColRef
=>
cmd `bmb` cr :: Cmd (Maybe a)


-- -}

-- ----------------------------------------

colHref :: String -> ColRef -> Cmd FilePath
colHref cf (i, cix) = do
  p <- objid2path i
  let fp =
        case cix of
          Nothing ->
            p ^. isoString
          Just pos ->
            (p ^. isoString) </> (pos ^. isoPicNo)
  return $ path2href cf fp

-- ----------------------------------------

colImgOp :: Monoid a =>
            (ObjId -> Name -> Cmd a) ->
            (ObjId ->         Cmd a) ->
            ColRef -> Cmd a
colImgOp iop cop = maybeColRef cop iref
  where
    iref i pos = do
      cs <- getImgVals i theColEntries
      case cs ^? ix pos of
        Just (ImgRef j n) -> iop j n
        _ -> return mempty

-- ----------------------------------------

colImgMeta :: ColRef -> Cmd MetaData
colImgMeta = colImgMeta' True

colImgMeta0 :: ColRef -> Cmd MetaData
colImgMeta0 = colImgMeta' False

colImgMeta' :: Bool -> ColRef -> Cmd MetaData
colImgMeta' gm = colImgOp iop cop
  where
    iop i _n
      | gm        = getMetaData i              -- exif data and descr
      | otherwise = getImgVals  i theMetaData  -- only descr
    cop i         = getImgVals  i theMetaData

colImgBlog :: ColRef -> Cmd Text
colImgBlog = maybeColRef cref (\ _ _ -> return mempty)
  where
    cref i = do
      nd <- getImgVals i theColBlog
      case nd of
        Just (j, n) -> getColBlogCont j n
        Nothing     -> return mempty

-- ----------------------------------------

colImgType :: ColRef -> Cmd ImgType
colImgType = colImgOp iop cop
  where
    iop i _n = do
      ity <$> getImgVals i theParts
      where
        ity ps
          | thePartNames' (`elem` [IMGimg, IMGjpg]) `has` ps = IMGjpg
          | thePartNames' (== IMGtxt)               `has` ps = IMGtxt
          | otherwise                                        = IMGother
    cop _i = return IMGjpg

colImgName :: ColRef -> Cmd (Maybe Name)
colImgName =
  colImgOp
  (\ _i n -> return $ Just n)
  (\ _i   -> return Nothing)

colImgPath :: ColRef -> Cmd (Maybe FilePath)
colImgPath = colImgOp iop cop
  where
    cop i = do -- col ref
      j'img <- getImgVals i theColImg
      case j'img of
        -- collection has a front page image
        Just (k, n) ->
          Just <$> buildImgPath k n
        _ ->
          return Nothing

    iop j n = Just <$> buildImgPath j n

buildImgPath :: ObjId -> Name -> Cmd FilePath
buildImgPath i n = do
  p <- objid2path i
  return $ substPathName n' p ^. isoString
 where
    -- if the image isn't a .jpg (.png, .gif, ...) then a .jpg is added
    n' | ".jpg" `isNameSuffix` n = n
       | otherwise               = substNameSuffix "" ".jpg" n

-- compute the image ref of a collection
-- if collection has an front page image, take that
-- otherwise take the ref to a generated image

colImgRef :: ObjId -> Cmd FilePath
colImgRef i = do
  p <- colImgPath (i, Nothing)
  maybe (iconRef i) return p

-- ----------------------------------------

colBlogCont :: ImgType -> ColRef -> Cmd Text
colBlogCont IMGtxt cr = do
  colImgOp iop cop cr
  where
    iop i n     = getColBlogCont i n
    cop _       = return mempty
colBlogCont _ _ = return mempty

getColBlogCont :: ObjId -> Name -> Cmd Text
getColBlogCont i n = do
      p <- objid2path i
      -- subst the name by the part name
      -- and build a file path
      f <- toFilePath (substPathName n p)
      genBlogHtml f

getColBlogSource :: ObjId -> Name -> Cmd Text
getColBlogSource i n = do
  p <- objid2path i
  -- subst the name by the part name
  -- and build a file path
  f <- toFilePath (substPathName n p)
  genBlogText f

putColBlogSource :: Text -> ObjId -> Name -> Cmd ()
putColBlogSource t i n = do
  p <- objid2path i
  f <- toFilePath (substPathName n p)
  writeBlogText t f
  journalChange $ SaveBlogText i n t

-- ----------------------------------------

-- compute the navigation hrefs for previous, next and parent image/collection

path2href :: String -> FilePath -> FilePath
path2href c p = "/" ++ c ++ p ++ ".html"

-- compute the icon ref of a collection

iconRef :: ObjId -> Cmd FilePath
iconRef i = do
  t  <- getImgVals i (theMetaData . metaDataAt descrTitle . isoString)

  mbf <-
    if isempty t  -- no title there
    then do
      p <- (^. isoString) <$> objid2path i
      path2img p
    else
      genAssetIcon (t'hash t) t

  return $
    fromMaybe (ps'blank ^. isoString) mbf
  where
    t'hash t' =
      mkCheckSum t' ^. isoString . to toP
      where
        toP s = x ++ "/" ++ y
          where
            (x, y) = splitAt 2 s

-- ----------------------------------------

-- aux template commands

addDefaultAct :: TmplEnv Cmd -> TmplEnv Cmd
addDefaultAct =
  insAct "*" (do n <- askTmplName
                 liftTA $ warn $ "evalTmpl: unknown template ref ignored: "
                                 ++ show (n ^. isoString)
                 empty
             )

blankImg :: Maybe FilePath -> ActCmd Text
blankImg f =
  xtxt $ fromMaybe ps'blank f

blankIcon :: Maybe ColRef -> Maybe FilePath -> ActCmd Text
blankIcon _ (Just f) =
  -- image there
  xtxt f

blankIcon (Just (i, Nothing)) _ = do
  -- ref to a collection, try to generate a collection icon
  liftTA (iconRef i) >>= xtxt

blankIcon _ _ =
  -- image not there
  xtxt ps'blank

path2img :: FilePath -> Cmd (Maybe FilePath)
path2img f
  | [("year", y)] <- m1 =
      genAssetIcon y y

  | [("year", y), ("month", m)] <- m1 =
      let s = toN m ++ "." ++ y
      in
        genAssetIcon (y </> s) s

  | [("year", y), ("month", m), ("day", d)] <- m1 =
      let s = toN d ++ "." ++ toN m ++ "." ++ y
      in
        genAssetIcon (y </> s) s

  | [("name", n)] <- m2 =
      genAssetIcon n n

{-
  | f == ps'bycreatedate =           -- "/archive/collections/byCreateDate"
      genAssetIcon s'bycreatedate (tt'bydate ^. isoString)

  | f == ps'clipboard =              -- "/archive/collections/photos/clipboard"
      genAssetIcon s'clipboard (tt'clipboard ^. isoString)

  | f == ps'trash =              -- "/archive/collections/photos/trash"
      genAssetIcon s'trash (tt'trash ^. isoString)

  | f == ps'photos =                 -- "/archive/collections/photos"
      genAssetIcon s'photos (tt'photos ^. isoString)

  | f == ps'collections =            -- "/archive/collections"
      genAssetIcon s'collections (tt'collections ^. isoString)
-- -}

  | otherwise =
      return Nothing
  where
    m1 = matchSubexRE ymdRE f
    m2 = matchSubexRE dirRE f

    toN :: String -> String
    toN s = show i
      where
        i :: Int
        i = read s


ymdRE :: Regex -- for collections sorted by date
ymdRE =
  parseRegexExt $
  ps'bycreatedate                    -- "/archive/collections/byCreateDate"
  ++
  "/({year}[0-9]{4})"
  ++
  "(/([-0-9]*({month}[0-9]{2}))(/([-0-9]*({day}[0-9]{2})))?)?"

dirRE :: Regex -- for collections for all folders
dirRE =
  parseRegexExt $
  ps'photos                          -- "/archive/collections/photos"
  ++
  "(/[^/]+)*"
  ++
  "(/({name}[^/]+))"

-- ----------------------------------------
