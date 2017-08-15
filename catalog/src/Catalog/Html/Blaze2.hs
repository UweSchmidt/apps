{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Catalog.Html.Blaze2
  ( genBlazeHtmlPage )
where

import Catalog.Cmd
import Data.ImgTree
import Data.MetaData
import Data.Prim

import Catalog.Html.Basic ( ColRef
                          , ColRefPath
                          , colBlogCont
                          , getColBlogCont
                          , colImgName
                          , colImgOp
                          , colImgPath
                          , colImgType
                          , iconRef
                          , maybeColRef
                          , mkColRefC
                          , mkColRefI
                          )
import Catalog.Html.Templates.Blaze2
import Catalog.System.ExifTool (getMetaData)

-- ----------------------------------------

-- type Html = [Text]

-- a page config has an name, a geo for the pictures,
-- a geo for the icons,
-- and the # icons per row

type PageConfig = (String, (GeoAR, GeoAR, Int))

-- an entry in a collection is identified by an ObjId, Path, Filepath, ...
-- and, an index into the list of collection entries, or it's a reference
-- to the collection itself

-- type ActCmd      = TmplAct Cmd

-- ----------------------------------------

thePageConfigs :: [PageConfig]
thePageConfigs =
  [ ("blaze-2560x1440", ( GeoAR 2560 1440 Pad
                        , GeoAR  160  120 Fix
                        , 14
                        )
    )
  , ("blaze-1920x1200", ( GeoAR 1920 1200 Pad
                        , GeoAR  160  120 Fix
                        , 11
                        )
    )
  , ("blaze-1600x1200", ( GeoAR 1600 1200 Pad
                        , GeoAR  160  120 Fix
                        , 9
                        )
    )
  , ("blaze-1400x1050", ( GeoAR 1400 1050 Pad
                        , GeoAR  140  105 Fix
                        , 9
                        )
    )
  , ("blaze-1280x800",  ( GeoAR 1280  800 Pad
                        , GeoAR  120   90 Fix
                        , 9
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

url2confPathNo :: FilePath -> Cmd (String, ColRefPath)
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
url2pathNo :: FilePath -> Cmd ColRefPath
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
crPath2crObjId :: ColRefPath -> Cmd (Maybe ColRef)
crPath2crObjId (p, cix) = do
  i <- fst <$> getIdNode' p
  normColRef (i, cix)

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

genBlazeHtmlPage :: FilePath -> Cmd Text
genBlazeHtmlPage p =
  renderPage <$> genBlazeHtmlPage' p

genBlazeHtmlPage' :: FilePath -> Cmd Html
genBlazeHtmlPage' p = do
  ( pageConf,
    this'cr@(this'i, pos)) <- url2confObjId p

  (geo1, geo2, no'cols) <- fromJustCmd
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
{-
  let noTxtRef ty ref
        | ty == IMGtxt = mempty
        | otherwise    = ref
  let next'href'    = noTxtRef next'type next'href
  let prev'href'    = noTxtRef prev'type prev'href
-}
  let getMD md n    = md ^. metaDataAt n
  let getTitle md   = getMD md descrTitle

  let this'title    = take1st [ getTitle this'meta
                              , this'fname ^. isoText
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

  theDate           <- ((^. isoText) . show) <$> atThisMoment
  let theTitle       = this'title
  let theSubTitle    = this'subtitle
  let theComment     = this'comment
  let theDuration    = this'duration
  let thisHref       = p             ^. isoText
  let thisPos        = this'ix       ^. isoText
  let theNextHref    = next'href     ^. isoText
  let thePrevHref    = prev'href     ^. isoText
  let theParentHref  = parent'href   ^. isoText
  let theChild1Href  = child1'href   ^. isoText
  let theImgGeo      = geo1 ^. theGeo
  -- let theIconGeo     = geo2 ^. theGeo
  let theImgGeoDir   = geo1 ^. isoString ^. isoText
  let theIconGeoDir  = geo2 ^. isoString ^. isoText
  thisImgRef        <- (^. isoText) <$> blankIcon (Just this'cr) this'img
  parentImgRef      <- (^. isoText) <$> blankIcon parent'cr      parent'img
  nextImgRef        <- (^. isoText) <$> blankIcon next'cr        next'img
  prevImgRef        <- (^. isoText) <$> blankIcon prev'cr        prev'img
  child1ImgRef      <- (^. isoText) <$> blankIcon child1'cr      child1'img
  let cBlogContents  = this'cblog    ^. isoText
  let blogContents   = this'blog     ^. isoText
  let theParentTitle = parent'title  ^. isoText
  let theNextTitle   = next'title    ^. isoText
  let thePrevTitle   = prev'title    ^. isoText
  let theChild1Title = child1'title  ^. isoText
  let metaData       = this'meta -- add jpg filename
                       & metaDataAt "File:RefJpg" .~ (this'fname ^. isoText)

  let toImgDescr (cr, href, img, title, pno) = do
        iref <- blankIcon cr img
        return ( href ^. isoText
               , iref ^. isoText
               , ( if isempty title
                   then (show (pno + 1) ++ ".Bild") ^. isoText
                   else title
                 )
               , pno ^. isoPicNo ^. isoText
               )
  theChildren      <- mapM toImgDescr children'5

  let page
        -- no position there: its a collection
        | isNothing pos
          = colPage'
            theTitle theDate
            theTitle theSubTitle theComment
            theImgGeo
            theDuration thisHref thisPos
            theNextHref thePrevHref theParentHref theChild1Href
            theImgGeoDir theIconGeoDir thisImgRef nextImgRef prevImgRef child1ImgRef
            cBlogContents
            theParentTitle parentImgRef
            theNextTitle thePrevTitle theChild1Title
            no'cols theChildren

        -- type is IMGtxt, so its a blog page
        | this'type == IMGtxt
          = txtPage'
            theTitle theDate
            theDuration thisHref thisPos
            theNextHref thePrevHref theParentHref
            theImgGeoDir nextImgRef prevImgRef
            blogContents

        -- its a picture page
        | otherwise
          = picPage'
            theTitle theDate
            theTitle theSubTitle theComment
            theImgGeo
            theDuration thisHref thisPos
            theNextHref thePrevHref theParentHref
            theImgGeoDir thisImgRef nextImgRef prevImgRef
            metaData

  return page

-- ----------------------------------------
--
-- a little helper for avoiding MaybeT transformer monad

bmb :: (Monad m, Monoid b) => (a -> m b) -> Maybe a -> m b
bmb cmd = maybe (return mempty) cmd

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

-- compute the navigation hrefs for previous, next and parent image/collection

path2href :: String -> FilePath -> FilePath
path2href c p = "/" ++ c ++ p ++ ".html"

-- ----------------------------------------

blankImg :: Maybe FilePath -> FilePath
blankImg f =
  fromMaybe ps'blank f

blankIcon :: Maybe ColRef -> Maybe FilePath -> Cmd FilePath
blankIcon _ (Just f) =
  -- image there
  return f

blankIcon (Just (i, Nothing)) _ =
  -- ref to a collection, try to generate a collection icon
  iconRef i

blankIcon _ Nothing =
  return ""

blankIcon _ _ =
  -- image not there
  return ps'blank

-- ----------------------------------------
