{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Catalog.Html.Blaze2
  ( PageConfig
  , genBlazeHtmlPage
  , genBlazeHtmlPage'
  , parseImgGeoPath
  )
where

import Data.Maybe
import System.FilePath

import Data.ImgTree
import Data.MetaData
import Data.Prim

import Catalog.Cmd
import Catalog.FilePath   ( addJpg
                          , blazePath'
                          , isoPicNo
                          )
import Catalog.Html.Basic ( ColRef
                          , ColRefPath
                          , colBlogCont
                          , getColBlogCont
                          , colImgName
                          , colImgOp
                          , colImgPath
                          , colImgPath0
                          , colImgType
                          , iconRef
                          , isPanorama
                          , maybeColRef
                          , mkColRefC
                          , mkColRefI
                          )
import Catalog.Html.Templates.Blaze2
import Catalog.System.ExifTool (getMetaData)
import Catalog.System.Convert  (getColImgSize)

import Text.SimpleParser

-- ----------------------------------------

-- type Html = [Text]

-- a page config has an name, a geo for the pictures,
-- a geo for the icons,
-- and the # icons per row

type PageConfig     = (GeoAR, GeoAR, Int)

-- an entry in a collection is identified by an ObjId, Path, Filepath, ...
-- and, an index into the list of collection entries, or it's a reference
-- to the collection itself

-- type ActCmd      = TmplAct Cmd

-- ----------------------------------------

thePageConfigs :: [PageConfig]
thePageConfigs =
  [ ( GeoAR 2560 1440 Pad
    , GeoAR  160  120 Fix
    , 14
    )
  , ( GeoAR 1920 1200 Pad
    , GeoAR  160  120 Fix
    , 11
    )
  , ( GeoAR 1600 1200 Pad
    , GeoAR  160  120 Fix
    , 9
    )
  , ( GeoAR 1400 1050 Pad
    , GeoAR  140  105 Fix
    , 9
    )
  , ( GeoAR 1280  800 Pad
    , GeoAR  120   90 Fix
    , 9
    )
  ]

lookupPageConfigs :: Geo -> Maybe PageConfig
lookupPageConfigs (Geo w h) =
  listToMaybe $
  filter (\(GeoAR w1 h1 _, _, _) -> w == w1 && h == h1) thePageConfigs

-- ----------------------------------------
--
-- url parsing

parseGeoPath :: FilePath -> Maybe (Geo, FilePath)
parseGeoPath = parseMaybe blazePath'

-- --------------------

parseImgPath :: FilePath -> Maybe (Path, Maybe Int)
parseImgPath fp0
  | not isHtml   = mzero
  | isJust picNo = return (readPath dp, picNo)
  | otherwise    = return (readPath fp, picNo)
  where
    (fp, ex) = splitExtension fp0
    isHtml   = ex == ".html"
    (dp, bn) = takeDirectory &&& takeFileName $ fp
    picNo    = (\ i -> if i >= 0 then Just i else Nothing) $
               bn ^. from isoPicNo

-- --------------------

parseImgGeoPath :: Geo -> FilePath -> Maybe (PageConfig, (Path, Maybe Int))
parseImgGeoPath geo fp = do
  p'i    <- parseImgPath fp
  pconf <- lookupPageConfigs geo
  return (pconf, p'i)

-- --------------------
--
-- scotty url parser

parseImgGeoPathPic :: FilePath -> Maybe (PageConfig, (Path, Maybe Int))
parseImgGeoPathPic fp = do
  (geo, fp1) <- parseGeoPath fp
  parseImgGeoPath geo fp1

-- ----------------------------------------
--
-- normalize a colref
-- if the result sub index is Nothing, the ref points to a collection
-- else the ref points to an image

normColRef :: ColRef -> Cmd (Maybe ColRef)
normColRef = maybeColRef cref iref
  where
    cref i = return $ Just $ mkColRefC i
    iref i pos = do
      val <- getImgVal i
      return
        ( ( colEntry'
            (\ _  -> mkColRefI i pos) -- ref to an image
            mkColRefC                 -- ref to a sub collection
          )
          <$>
          (val ^? theColEntries . ix pos)  -- a Maybe value
        )

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

data PageType = IsCol | IsPic | IsTxt

-- ----------------------------------------
--
-- the main entry for a HTML page scotty variant

genBlazeHtmlPage :: FilePath -> Cmd LazyText
genBlazeHtmlPage f
  | Just (pconf, colref) <- parseImgGeoPathPic f =
      renderPage <$> genBlazeHtmlPage' pconf colref
  | otherwise =
      abort $ "genBlazeHtmlPage: not found: " ++ show f

-- ----------------------------------------
--
-- the main entry for the servant variant

genBlazeHtmlPage' :: PageConfig -> ColRefPath -> Cmd Html
genBlazeHtmlPage' (geo1, geo2, no'cols)
                   (this'path, pos) = do
  this'i <- fst <$> getIdNode' this'path

  let this'cr = (this'i, pos)
  let pageConf = "blaze-" ++ geo1 ^. theGeo . isoString
  let p = "/" ++ pageConf ++
          this'path ^. isoString ++
          maybe "" (\ pos' -> "/" ++ pos' ^. isoPicNo) pos ++
          ".html"

  theDate <- ((^. isoText) . show) <$> atThisMoment

  -- this entry
  this'type   <- colImgType               this'cr
  this'img0   <- colImgPath0              this'cr
  let this'img = addJpg <$> this'img0
  this'fname  <- ((^. from isoMaybe) . fmap (^. isoString)) <$>
                 colImgName               this'cr

  let pageType
        | isNothing pos       = IsCol
        | this'type == IMGtxt = IsTxt
        | otherwise           = IsPic

  -- for a col: get the entries, for an img: empty
  this'cs     <- getImgVals this'i theColEntries
  this'meta   <- colImgMeta               this'cr
  this'ix     <- maybe "" (^. isoPicNo)
                             <$> ixColRef this'cr

  parent'cr   <- parentColRef             this'cr
  parent'href <- colHref pageConf   `bmb` parent'cr

  prev'cr     <- prevColRef               this'cr
  prev'href   <- colHref pageConf   `bmb` prev'cr
  prev'img    <- colImgPath         `bmb` prev'cr
  prev'meta   <- colImgMeta0        `bmb` prev'cr

  next'cr     <- nextColRef               this'cr
  next'href   <- colHref pageConf   `bmb` next'cr
  next'img    <- colImgPath         `bmb` next'cr
  next'meta   <- colImgMeta0        `bmb` next'cr

  let getMD md n    = md ^. metaDataAt n
  let getTitle md   = getMD md descrTitle

  let theTitle       = take1st [ getTitle this'meta
                               , this'fname ^. isoText
                               ]
  let theSubTitle    = getMD this'meta descrSubtitle
  let theComment     = getMD this'meta descrComment
  let theDuration    = take1st [ getMD this'meta descrDuration
                               , "1.0"
                               ]
  let thisHref       = p             ^. isoText
  let thisPos        = this'ix       ^. isoText
  let theNextHref    = next'href     ^. isoText
  let thePrevHref    = prev'href     ^. isoText
  let theParentHref  = parent'href   ^. isoText
  let theImgGeo      = geo1 ^. theGeo
  let theImgGeoDir   = geo1 ^. isoText

  thisImgRef        <- (^. isoText) <$> blankIcon (Just this'cr) this'img
  nextImgRef        <- (^. isoText) <$> blankIcon next'cr        next'img
  prevImgRef        <- (^. isoText) <$> blankIcon prev'cr        prev'img

  let theNextTitle   = getTitle next'meta
  let thePrevTitle   = getTitle prev'meta

  case pageType of
    IsCol -> do
      let theIconGeoDir  = geo2 ^. isoString ^. isoText

      theParentTitle <- getTitle <$> (colImgMeta0 `bmb` parent'cr)
      parent'img     <- colImgPath `bmb` parent'cr
      parentImgRef   <- (^. isoText) <$> blankIcon parent'cr parent'img

      child1'cr      <- childColRef 0            this'cr
      child1'img     <- colImgPath         `bmb` child1'cr

      child1ImgRef   <- (^. isoText) <$> blankIcon child1'cr child1'img
      theChild1Href  <- (^. isoText) <$> (colHref pageConf `bmb` child1'cr)
      theChild1Title <- getTitle     <$> (colImgMeta0      `bmb` child1'cr)

      cBlogContents  <- (^. isoText) <$> colImgBlog this'cr

      children'crs   <- mapM (flip childColRef this'cr) $
                         [0 .. length this'cs - 1]
      children'hrefs <- mapM (colHref pageConf `bmb`) children'crs
      children'imgs  <- mapM (colImgPath       `bmb`) children'crs
      children'meta  <- mapM (colImgMeta0      `bmb`) children'crs

      let children'titles = map getTitle children'meta
      let children'5      = zip5
                            children'crs
                            children'hrefs
                            children'imgs
                            children'titles
                            [(0::Int)..]

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
      theChildren <- mapM toImgDescr children'5

      return $
        colPage'
        "/"
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

    IsTxt -> do
      blogContents <- (^. isoText) <$> colBlogCont this'type this'cr

      return $
        txtPage'
        "/"
        theTitle theDate
        theDuration thisHref thisPos
        theNextHref thePrevHref theParentHref
        theImgGeoDir nextImgRef prevImgRef
        blogContents

    IsPic -> do
      let star     = '\9733'
      let rating   = (\ r -> replicate r star ^. isoText) $
                     getRating this'meta

      let metaData = this'meta -- add jpg filename
                     & metaDataAt "File:RefJpg" .~ (this'fname ^. isoText)
                     & metaDataAt "Img:Rating"  .~ rating

      orgImgGeo   <- maybe (return mempty) getColImgSize $ this'img0
      let thePanoGeoDir
                  = (^. isoText) <$> isPanorama theImgGeo orgImgGeo

      return $
        picPage'
        "/"
        theTitle theDate
        theTitle theSubTitle theComment
        theImgGeo thePanoGeoDir
        theDuration thisHref thisPos
        theNextHref thePrevHref theParentHref
        theImgGeoDir thisImgRef nextImgRef prevImgRef "" ""
        metaData

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
      nd <- getImgVal i -- theColBlog
      case nd ^? theColBlog . traverse of
        Just ir -> getColBlogCont ir
        Nothing -> return mempty

-- ----------------------------------------

-- compute the navigation hrefs for previous, next and parent image/collection

path2href :: String -> FilePath -> FilePath
path2href c p = "/" ++ c ++ p ++ ".html"

-- ----------------------------------------

-- blankImg :: Maybe FilePath -> FilePath
-- blankImg f = fromMaybe ps'blank f

blankIcon :: Maybe ColRef -> Maybe FilePath -> Cmd FilePath
blankIcon _ (Just f) =
  -- image there
  return f

blankIcon (Just (i, Nothing)) _ =
  -- ref to a collection, try to generate a collection icon
  iconRef i

blankIcon _ Nothing =
  return ""

-- ----------------------------------------
