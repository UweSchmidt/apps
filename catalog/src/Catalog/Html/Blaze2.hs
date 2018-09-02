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

normColRef :: ColRef -> CmdMB ColRef
normColRef = maybeColRef cref iref
  where
    cref i     = return $ mkColRefC i
    iref i pos = do
      val <- lift $ getImgVal i
      pureMB
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

parentColRef :: ColRef -> CmdMB ColRef
parentColRef = maybeColRef cref iref
  where
    cref i =  do
      parent'i <- lift $ getImgParent i
      iscol    <- lift $ isCOL <$> getImgVal parent'i
      if iscol
        then return $ mkColRefC parent'i
        else mzero

    iref i _pos = return $ mkColRefC i

childColRef :: Int -> ColRef -> CmdMB ColRef
childColRef pos = maybeColRef cref iref
  where
    cref i = do
      _ix <- (^? ix pos) <$> lift (getImgVals i theColEntries)
      normColRef $ mkColRefI i pos

    iref _i _pos = mzero -- pictures don't have children

ixColRef :: ColRef -> CmdMB Int
ixColRef = maybeColRef cref iref
  where
    cref i = do
      (j, _) <- parentColRef $ mkColRefC i
      cs     <- lift $ getImgVals j theColEntries
      pureMB $  searchPos ((== i) . (^. theColObjId)) cs

    iref _i pos = return pos

neighborColRef :: Int -> ColRef -> CmdMB ColRef
neighborColRef offset = maybeColRef cref iref
  where
    cref i = do
      let cr = mkColRefC i
      pos       <- ixColRef     cr         -- position in parent col
      parent'cr <- parentColRef cr         -- 1 level up
      childColRef (pos + offset) parent'cr -- 1 level down

    iref i pos = normColRef $ mkColRefI i (pos + offset)


prevColRef :: ColRef -> CmdMB ColRef
prevColRef = neighborColRef (-1)

nextColRef :: ColRef -> CmdMB ColRef
nextColRef = neighborColRef   1

-- right traversal through the whole tree

nextOrUpColRef :: ColRef -> CmdMB ColRef
nextOrUpColRef ref =
  nextColRef ref
  <|>
  ( parentColRef ref
    >>=
    nextOrUpColRef
  )

fwrdColRef :: ColRef -> CmdMB ColRef
fwrdColRef ref =
      childColRef 0 ref  -- down to the 1. child
      <|>                -- or if empty collection
      nextOrUpColRef ref -- to the right or up

-- ----------------------------------------

data PageType = IsCol | IsPic | IsTxt

thePageType :: ColRef -> Cmd PageType
thePageType cr@(_i, pos)
  | isNothing pos =
      return IsCol

  | otherwise = do
      ty <- colImgType cr
      return $ case ty of
        IMGtxt -> IsTxt
        _      -> IsPic

colImgPath' :: ColRef -> Cmd (Maybe FilePath)
colImgPath' cr = do
      ty <- thePageType cr
      case ty of
        IsPic -> colImgPath cr
        _     -> return mempty

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

  pageType    <- thePageType this'cr

  -- for a col: get the entries, for an img: empty
  this'cs     <- getImgVals this'i theColEntries
  this'meta   <- colImgMeta                 this'cr
  this'ix     <- runMB $
                (^. isoPicNo) <$> ixColRef  this'cr

  parent'cr   <- runMaybeT $ parentColRef   this'cr
  parent'href <- colHref pageConf   `appMB` parent'cr

  prev'cr     <- runMaybeT $ prevColRef     this'cr
  prev'href   <- colHref pageConf   `appMB` prev'cr
  prev'img    <- colImgPath'        `appMB` prev'cr
  prev'meta   <- colImgMeta0        `appMB` prev'cr

  next'cr     <- runMaybeT $ nextColRef     this'cr
  next'href   <- colHref pageConf   `appMB` next'cr
  next'img    <- colImgPath'        `appMB` next'cr
  next'meta   <- colImgMeta0        `appMB` next'cr

  fwrd'cr     <- runMaybeT $ fwrdColRef     this'cr
  fwrd'href   <- colHref pageConf   `appMB` fwrd'cr
  fwrd'img    <- colImgPath'        `appMB` fwrd'cr

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
  let theFwrdHref    = fwrd'href     ^. isoText
  let theParentHref  = parent'href   ^. isoText
  let theImgGeo      = geo1 ^. theGeo
  let theImgGeoDir   = geo1 ^. isoText

  let iscol cr' = maybe False (isNothing . snd ) cr'

  thisImgRef        <- (^. isoText) <$> blankIcon (Just this'cr) this'img
  nextImgRef        <- (^. isoText) <$> blankIcon next'cr        next'img
  prevImgRef        <- (^. isoText) <$> blankIcon prev'cr        prev'img
  fwrdImgRef        <- if iscol fwrd'cr
                          then return mempty
                          else (^. isoText) <$>
                               blankIcon fwrd'cr fwrd'img

  let theNextTitle   = getTitle next'meta
  let thePrevTitle   = getTitle prev'meta

  case pageType of
    IsCol -> do
      let theIconGeoDir  = geo2 ^. isoString ^. isoText

      theParentTitle <- getTitle <$>
                        (colImgMeta0 `appMB`       parent'cr)
      parent'img     <-  colImgPath  `appMB`       parent'cr
      parentImgRef   <- (^. isoText) <$> blankIcon parent'cr parent'img

      child1'cr      <- runMaybeT $ childColRef 0 this'cr
      child1'img     <- colImgPath         `appMB` child1'cr

      child1ImgRef   <- (^. isoText) <$> blankIcon child1'cr child1'img
      theChild1Href  <- (^. isoText) <$> (colHref pageConf `appMB` child1'cr)
      theChild1Title <- getTitle     <$> (colImgMeta0      `appMB` child1'cr)

      cBlogContents  <- (^. isoText) <$> colImgBlog this'cr

      children'crs   <- mapM
                        (\ i -> runMaybeT $ childColRef i this'cr)
                        [0 .. length this'cs - 1]
      children'hrefs <- mapM (colHref pageConf `appMB`) children'crs
      children'imgs  <- mapM (colImgPath       `appMB`) children'crs
      children'meta  <- mapM (colImgMeta0      `appMB`) children'crs

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
        theFwrdHref
        theImgGeoDir theIconGeoDir
        thisImgRef nextImgRef prevImgRef child1ImgRef
        fwrdImgRef
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
        theNextHref thePrevHref theParentHref theFwrdHref
        theImgGeoDir
        nextImgRef prevImgRef fwrdImgRef
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
        theNextHref thePrevHref theParentHref theFwrdHref
        theImgGeoDir
        thisImgRef nextImgRef prevImgRef fwrdImgRef
        mempty mempty
        metaData

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
