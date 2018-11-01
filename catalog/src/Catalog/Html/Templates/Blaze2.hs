{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Catalog.Html.Templates.Blaze2
  ( Html
  , IconDescr
  , picPage'
  , colPage'
  , txtPage'
  , renderPage
  , renderPage'
  )
where

import           Data.MetaData ( MetaData
                               , metaDataAt
                               , loc2googleMapsUrl

                               , compositeDOF
                               , compositeGPSAltitude
                               , compositeGPSLatitude
                               , compositeGPSLongitude
                               , compositeGPSPosition
                               , compositeImageSize
                               , compositeLensID
                               , compositeLensSpec

                               , descrComment
                               , descrSubtitle
                               , descrTitle
                               , descrTitleEnglish
                               , descrTitleLatin
                               , descrWeb
                               , descrWikipedia

                               , exifCreateDate
                               , exifExposureCompensation
                               , exifExposureMode
                               , exifExposureProgram
                               , exifExposureTime
                               , exifFNumber
                               , exifFocalLength
                               , exifFocalLengthIn35mmFormat
                               , exifISO
                               , exifModel
                               , exifWhiteBalance

                               , fileFileModifyDate
                               , fileFileName
                               , fileRefJpg

                               , imgRating

                               , makerNotesFocusDistance
                               , makerNotesShootingMode
                               , makerNotesShutterCount
                               )
import           Data.Prim
import qualified Data.Text                       as T
import           Text.Blaze.Html5                hiding (map, head)
import qualified Text.Blaze.Html5                as H
import           Text.Blaze.Html5.Attributes     hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes     as A
import qualified Text.Blaze.Html.Renderer.Pretty as R
import qualified Text.Blaze.Html.Renderer.Text   as T
import qualified Text.SimpleParser               as SP

renderPage' :: Html -> LazyText
renderPage' p = T.renderHtml p

-- indent HTML
renderPage :: Html -> LazyText
renderPage p = R.renderHtml p ^. isoText . lazy

-- ----------------------------------------

type IconDescr = (Text, Text, Text, Text)

colPage' :: Text -> Text -> Text
         -> Text -> Text -> Text
         -> Geo
         -> Text -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> Text -> Text
         -> Text -> Text -> Text -> Text -> Text
         -> Text
         -> Text -> Text
         -> Text -> Text -> Text
         -> Int  -> [IconDescr]
         -> Html
colPage'
  theBaseRef theHeadTitle theDate
  theTitle theSubTitle theComment
  theImgGeo
  theDuration thisHref thisPos
  theNextHref thePrevHref theParentHref theChild1Href theFwrdHref
  theImgGeoDir theIconGeoDir
  thisImgRef nextImgRef prevImgRef child1ImgRef fwrdImgRef
  cBlogContents
  theParentTitle parentImgRef
  theNextTitle thePrevTitle theChild1Title
  no'cols icons

  = colPage
    theBaseRef
    theHeadTitle
    theDate
    theImgGeo
    ( jsCode
      theDuration
      thisHref
      thisPos
      theNextHref
      thePrevHref
      theParentHref
      theChild1Href
      theFwrdHref
      ( imgRef theImgGeoDir nextImgRef )
      ( imgRef theImgGeoDir prevImgRef )
      ( imgRef theImgGeoDir fwrdImgRef )
      mempty
      mempty
      mempty
    )
    ( colImg
      theImgGeoDir theIconGeoDir
      thisImgRef
      theHeadTitle
    )
    ( colTitle
      theTitle
      theSubTitle
      theComment
    )
    ( colNav
      ( parentNav theParentTitle parentImgRef  theImgGeoDir theIconGeoDir)
      ( prevNav   thePrevTitle   prevImgRef    theImgGeoDir theIconGeoDir)
      ( child1Nav theChild1Title child1ImgRef
                                 theChild1Href theImgGeoDir theIconGeoDir)
      ( nextNav   theNextTitle   nextImgRef    theImgGeoDir theIconGeoDir)
    )
    ( colBlog
      cBlogContents
    )
    ( colContents
      no'cols
      ( map (colIcon theImgGeoDir theIconGeoDir) icons )
    )

picPage' :: Text -> Text -> Text
         -> Text -> Text -> Text
         -> Geo  -> Maybe Text
         -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> Text
         -> Text -> Text -> Text -> Text
         -> Text -> Text
         -> MetaData
         -> Html
picPage'
  theBaseRef theHeadTitle theDate
  theTitle theSubTitle theComment
  theImgGeo thePanoGeoDir
  theDuration thisHref thisPos
  theNextHref thePrevHref theParentHref theFwrdHref
  theImgGeoDir
  thisImgRef nextImgRef prevImgRef fwrdImgRef
  orgImgRef panoImgRef
  metaData
  = picPage
    theBaseRef
    theHeadTitle
    theDate
    theImgGeo
    ( jsCode
      theDuration
      thisHref
      thisPos
      theNextHref
      thePrevHref
      theParentHref
      mempty -- child1 href
      theFwrdHref
      ( imgRef theImgGeoDir nextImgRef )
      ( imgRef theImgGeoDir prevImgRef )
      ( imgRef theImgGeoDir fwrdImgRef )
      ( imgRef theImgGeoDir thisImgRef )
      -- hack to work with old and new url scheme
      ( if T.null orgImgRef
        then imgRef orgGeoDir thisImgRef
        else orgImgRef
      )
      -- hack to work with old and new url scheme
      ( if T.null panoImgRef
        then maybe mempty (flip imgRef thisImgRef) thePanoGeoDir
        else panoImgRef
      )
    )
    ( picImg
      theImgGeo
      theImgGeoDir
      thisImgRef
    )
    ( picTitle
      theImgGeo
      theTitle
      theSubTitle
      theComment
    )
    picNav
    ( picInfo
      theImgGeo
      metaData
    )

txtPage' :: Text -> Text -> Text
         -> Text -> Text -> Text
         -> Text -> Text -> Text -> Text
         -> Text
         -> Text -> Text -> Text
         -> Text
         -> Html
txtPage'
  theBaseRef theHeadTitle theDate
  theDuration thisHref thisPos
  theNextHref thePrevHref theParentHref theFwrdHref
  theImgGeoDir
  nextImgRef prevImgRef fwrdImgRef
  blogContents
  = txtPage
    theBaseRef
    theHeadTitle
    theDate
    ( jsCode
      theDuration
      thisHref
      thisPos
      theNextHref
      thePrevHref
      theParentHref
      mempty
      theFwrdHref
      ( imgRef theImgGeoDir nextImgRef )
      ( imgRef theImgGeoDir prevImgRef )
      ( imgRef theImgGeoDir fwrdImgRef )
      mempty mempty mempty
    )
    blogContents

-- ----------------------------------------

picPage :: Text -> Text -> Text
        -> Geo
        -> Html -> Html -> Html -> Html -> Html
        -> Html
picPage theBaseRef theHeadTitle theDate
        theImgGeo
        jsCode picImg picTitle picNav picInfo
  = htmlPage
    ( headPage theBaseRef theHeadTitle theDate jsCode ) $ do
  body
    ! onload "initPicture();"
    ! class_ (toValue $ "picture picture-" <> (theImgGeo ^. isoText)) $ do
    picImg
    picTitle
    picInfo
    picNav

-- ----------------------------------------

txtPage :: Text -> Text -> Text
        -> Html -> Text
        -> Html
txtPage theBaseRef theHeadTitle theDate
        jsCode blogContents
  = htmlPage
    ( headPage theBaseRef theHeadTitle theDate jsCode ) $ do
  body
    ! onload "initPicture();"
    ! class_ "text" $ preEscapedText blogContents

-- ----------------------------------------

colPage :: Text
        -> Text -> Text -> Geo
        -> Html -> Html -> Html -> Html -> Html -> Html
        -> Html
colPage theBaseRef theHeadTitle theDate theImgGeo
        jsCode colImg colTitle colNav colBlog colContents
  = htmlPage
    ( headPage theBaseRef theHeadTitle theDate jsCode ) $ do
  body
    ! onload "initAlbum();"
    ! class_ (toValue $ "album album-" <> theImgGeo ^. isoText)
    ! A.id   "theAlbumBody"
    $ do
    table ! class_ "head" $ do
      tr $ do
        td ! class_ "head1" $ colImg
        td ! class_ "head2" $ colTitle
        td ! class_ "head3" $ colNav
    colBlog
    H.div ! class_ "ruler" $ mempty
    H.div ! class_ "album-content" $ colContents
    H.div ! class_ "ruler" $ mempty
    table ! class_ "head" $ do
      tr $ do
        td ! class_ "head1" $ mempty
        td ! class_ "head2" $ mempty
        td ! class_ "head3" $ colNav

-- ----------------------------------------

headPage :: Text -> Text -> Text -> Html -> Html
headPage theBaseRef theHeadTitle theDate theJS
  = H.head $ do
  title $ toHtml theHeadTitle
  meta ! name "description" ! content "Web Photo Album"
  meta ! name "author"      ! content "Uwe Schmidt"
  meta ! name "generator"   ! content "catalog-server"
  meta ! name "date"        ! content (toValue theDate)
  base ! href (toValue theBaseRef)
  link
    ! rel   "shortcut icon"
    ! href  "/favicon.ico"
    ! type_ "image/x-icon"
  link
    ! rel   "stylesheet"
    ! type_ "text/css"
    ! href  "/assets/css/html-album.css"
  theJS
  script
    ! type_   "text/javascript"
    ! src     "/bootstrap/js/jquery/1.12.4/jquery.min.js"
    $ mempty
  script
    ! type_   "text/javascript"
    ! src     "/assets/javascript/html-album.js"
    $ mempty

htmlPage :: Html -> Html -> Html
htmlPage theHead theBody = do
  docType
  html
    ! lang "de" $ do
    theHead
    theBody

-- ----------------------------------------

colBlog :: Text -> Html
colBlog cBlogContents
  | T.null cBlogContents = mempty
  | otherwise = do
      H.div ! class_ "ruler" $ mempty
      H.div ! class_ "blog-contents" $ preEscapedText cBlogContents

jsCode :: Text -> Text -> Text
       -> Text -> Text -> Text
       -> Text -> Text
       -> Text -> Text -> Text
       -> Text -> Text -> Text
       -> Html
jsCode theDuration thisHref thisPos
      theNextHref thePrevHref theParentHref
      theChild1Href theFwrdHref
      theNextImgRef thePrevImgRef theFwrdImgRef
      thisImgRef thisOrgRef thisPanoRef
  = do
  script ! type_ "text/javascript" $ preEscapedText $ T.unlines $
    [ ""
    , "<!--"
    , "var duration = 7000 * '" <> theDuration <> "';"
    , "var thisp    = '" <> thisHref <> "';"
    , "var thispos  = '#" <> thisPos <> "';"
    , "var nextp    = '" <> theNextHref   <> "';"
    , "var prevp    = '" <> thePrevHref   <> "';"
    , "var fwrdp    = '" <> theFwrdHref   <> "';"
    , "var parentp  = '" <> theParentHref <> "';"
    , "var childp   = '" <> theChild1Href <> "';"
    , "var nextimg  = '" <> theNextImgRef <> "';"
    , "var previmg  = '" <> thePrevImgRef <> "';"
    , "var fwrdimg  = '" <> theFwrdImgRef <> "';"
    , "var thisimg  = '" <> thisImgRef    <> "';"
    , "var orgimg   = '" <> thisOrgRef    <> "';"
    , "var panoimg  = '" <> thisPanoRef   <> "';"
    , "-->"
    ]

colTitle :: Text -> Text -> Text -> Html
colTitle theTitle theSubTitle theComment = do
  di "title"    theTitle
  di "subtitle" theSubTitle
  di "comment"  theComment
  where
    di :: Text -> Text -> Html
    di c t
      | T.null t = mempty
      | otherwise =   H.div ! class_ (toValue c) $ toHtml t

colImg :: Text -> Text -> Text -> Text -> Html
colImg theImgGeoDir theIconGeoDir thisImgRef theHeadTitle =
  img ! src     (toValue $ imgRef' theImgGeoDir theIconGeoDir thisImgRef)
      ! class_  (toValue $ "icon-" <> theIconGeoDir)
      ! A.title (toValue theHeadTitle)
      ! alt     (toValue theHeadTitle)

colNav :: Html -> Html -> Html -> Html -> Html
colNav parentNav prevNav child1Nav nextNav = do
  table ! class_ "nav" {- ! align "right" -} $ do
    tr $ do
      td ! class_ "icon2" $ preEscapedText "&nbsp;"
      td ! class_ "icon2" ! A.id "theParentNav" $ parentNav
      td ! class_ "icon2" $ preEscapedText "&nbsp;"
    tr $ do
      td ! class_ "icon2" ! A.id "thePrevNav"   $ prevNav
      td ! class_ "icon2" ! A.id "theChild1Nav" $ child1Nav
      td ! class_ "icon2" ! A.id "theNextNav"   $ nextNav

parentNav :: Text -> Text -> Text -> Text -> Html
parentNav theParentTitle parentImgRef theImgGeoDir theIconGeoDir =
  nav' "javascript:parentPage();"
       ("Album" <:> theParentTitle)
       parentImgRef
       theImgGeoDir theIconGeoDir

nextNav :: Text -> Text -> Text -> Text -> Html
nextNav theNextTitle nextImgRef theImgGeoDir theIconGeoDir =
  nav' "javascript:nextPage();"
       ("weiter" <:> theNextTitle)
       nextImgRef
       theImgGeoDir theIconGeoDir

prevNav :: Text -> Text -> Text -> Text -> Html
prevNav thePrevTitle prevImgRef theImgGeoDir theIconGeoDir =
  nav' "javascript:prevPage();"
       ("zur\252ck" <:> thePrevTitle)
       prevImgRef
       theImgGeoDir theIconGeoDir

child1Nav :: Text -> Text -> Text -> Text -> Text -> Html
child1Nav theChild1Title child1ImgRef
                         theChild1Href theImgGeoDir theIconGeoDir =
  nav' ("javascript:childPage('" <> theChild1Href <> "');")
       ("1. Bild" <:> theChild1Title)
       child1ImgRef
       theImgGeoDir theIconGeoDir


nav' :: Text -> Text -> Text -> Text -> Text -> Html
nav' theHref theTitle theImgRef theImgGeoDir theIconGeoDir
  | T.null theImgRef = mempty
  | otherwise        = do
      a ! href (toValue theHref)
        ! A.title (toValue theTitle) $ do
        img ! src    (toValue $ imgRef' theImgGeoDir theIconGeoDir theImgRef)
            ! class_ (toValue $ "icon2-" <> theIconGeoDir)
            ! alt    (toValue theTitle)

-- ----------------------------------------

colContents :: Int -> [Html] -> Html
colContents no'cols colIcons = do
  table ! class_ "col-contents" $ mconcat $ map toRow colRows
  where
    colRows = divideAt no'cols colIcons
    toRow :: [Html] -> Html
    toRow r = do
      tr ! class_ "col-row" $ mconcat r

colIcon :: Text -> Text -> IconDescr -> Html
colIcon theImgGeoDir theIconGeoDir
       (theChildHref, theChildImgRef, theChildTitle, theChildId) = do
  td ! class_ (toValue $ "icon-" <> theIconGeoDir)
     ! A.id   (toValue theChildId)
     ! A.name (toValue theChildId) $ do
    H.a ! href    (toValue $ "javascript:childPage('" <> theChildHref <> "');")
        ! A.title (toValue theChildTitle) $ do
      img ! src    (toValue $ imgRef' theImgGeoDir theIconGeoDir theChildImgRef)
          ! class_ (toValue $ "icon-" <> theIconGeoDir)
          ! alt    (toValue theChildTitle)

-- ----------------------------------------

picImg :: Geo -> Text -> Text -> Html
picImg theImgGeo theImgGeoDir thisImgRef = do
  -- the scaled picture fitting on the dispay
  H.div ! class_   "picture"
        ! A.id     "pic-scaled" $
    table ! class_ "picture" $
      tr $
        td ! class_ "picture" $
          img ! src (toValue $ imgRef theImgGeoDir thisImgRef)

  -- the panorama picture, fitting the screen height
  H.div ! scroll "-x"
        ! class_ "panorama"
        ! A.id   "pic-pano" $
    img ! src    ""
        ! class_ "panorama"

  -- the picture in original size
  H.div ! scroll ""
        ! A.id   "pic-org" $
    img ! src    ""
  where
    scroll x =
      A.style ( toValue $
                   "overflow: scroll" <> x <> ";"
                <> "display:  none;"
                <> "width: "  <> theImgGeo ^. theW . isoText <> "px;"
                <> "height: " <> theImgGeo ^. theH . isoText <> "px;"
              )

picTitle :: Geo -> Text -> Text -> Text -> Html
picTitle theImgGeo theTitle theSubTitle theComment =
  H.div ! class_ "title-area"
        ! onmouseover "showTitle();"
        ! onmouseout  "hideTitle();" $
    H.div ! class_ (toValue $ "title-area-line title-area-line-" <>
                              (theImgGeo ^. isoText)
                   )
          ! A.id   "title-area-line" $ do
      H.div ! class_ "title"    $ toHtml theTitle
      H.div ! class_ "subtitle" $ toHtml theSubTitle
      H.div ! class_ "comment"  $ toHtml theComment

picNav :: Html
picNav = do
    H.a ! href "javascript:parentPage()"
        ! A.title "Album"
        ! class_ "up"
        ! A.id "theUpButton" $
      espan "theContainingAlbum"

    H.a ! href "javascript:prevPage()"
        ! A.title "zur\252ck"
        ! class_ "back"
        ! A.id "theBackButton" $
      espan "thePreviousPic"

    H.a ! href "javascript:nextPage()"
        ! A.title "weiter"
        ! class_ "forward"
        ! A.id "theForwardButton" $
      espan "theNextPic"
  where
    espan :: Text -> Html
    espan sid = H.span ! A.id (toValue sid) $
      preEscapedText "&nbsp;"

picInfo :: Geo -> MetaData -> Html
picInfo theImgGeo md =
    H.div ! class_ "info-area"
          ! onmouseover "showInfo();"
          ! onmouseout "hideInfo();" $
      H.div ! class_ (toValue $ "info-area-content info-area-content-" <>
                                theImgGeo ^. isoText
                     )
            ! A.id "info-area-content" $
        H.div ! class_ "info" $ do
          H.div ! class_ "subtitle" $ "Bild-Daten"
          table ! class_ "info" $ picMeta md

picMeta :: MetaData -> Html
picMeta md = mconcat $ map toMarkup mdTab
  where
    toMarkup (descr, key, process)
      | T.null val = mempty
      | otherwise  = toEntry descr key (process val)
      where
        val = md ^. metaDataAt key

    toEntry :: Text -> Name -> Html -> Html
    toEntry descr key val =
      tr ! class_ "info"
         ! A.id (toValue $ key ^. isoText) $ do
        th $ toHtml descr
        td $ val

    mdv :: Text -> Html
    mdv v = toHtml v

    mdWeb :: Text -> Html
    mdWeb v =
      H.a ! href (toValue v) $ toHtml v

    mdWiki :: Text -> Html
    mdWiki v =
      H.a ! href (toValue v) $ "--> Wikipedia"

    mdMap :: Text -> Html
    mdMap pos =
      H.a ! href (toValue gmhref) $ toHtml $ formatDegree pos
      where
        gmhref, gmloc :: Text
        gmhref = "https://maps.google.de/maps/@" <> gmloc <> ",17z"
        gmloc = loc2googleMapsUrl (pos ^. isoString) ^. from isoMaybe . isoText

    mdRating :: Text -> Html
    mdRating r =
      H.span ! A.style "color: red" $ toHtml r

    mdFile :: Text -> Html
    mdFile v =
      toHtml $ d <> "/" <> v
      where
        d = md ^. metaDataAt "File:Directory"

    mdLoc :: Text -> Html
    mdLoc v = toHtml $ formatDegree v

    -- subst " deg" by degree char '\176'
    formatDegree :: Text -> Text
    formatDegree t =
      (SP.sedP (const "\176") (SP.string " deg") $ t ^. isoString) ^. isoText

    mdTab :: [(Text, Name, Text -> Html)]
    mdTab =
      [ ("Titel",                descrTitle,                     mdv)
      , ("Untertitel",           descrSubtitle,                  mdv)
      , ("Titel (engl.)",        descrTitleEnglish,              mdv)
      , ("Titel (lat.)",         descrTitleLatin,                mdv)
      , ("Kommentar",            descrComment,                   mdv )
      , ("Web",                  descrWeb,                       mdWeb )
      , ("Wikipedia",            descrWikipedia,                 mdWiki )
      , ("Karte",                compositeGPSPosition,           mdMap )
      , ("Breitengrad",          compositeGPSLatitude,           mdLoc)
      , ("Längengrad",           compositeGPSLongitude,          mdLoc)
      , ("Höhe",                 compositeGPSAltitude,           mdv)
      , ("Aufnahmedatum",        exifCreateDate,                 mdv)
      , ("Kamera",               exifModel,                      mdv)
      , ("Objektiv",             compositeLensSpec,              mdv)
      , ("Objektiv Typ",         compositeLensID,                mdv)
      , ("Brennweite",           exifFocalLength,                mdv)
      , ("Brennweite in 35mm",   exifFocalLengthIn35mmFormat,    mdv)
      , ("Belichtungszeit",      exifExposureTime,               mdv)
      , ("Blende",               exifFNumber,                    mdv)
      , ("Belichtungskorrektur", exifExposureCompensation,       mdv)
      , ("ISO",                  exifISO,                        mdv)
      , ("Belichtungsmessung",   exifExposureMode,               mdv)
      , ("Aufnahmebetriebsart",  exifExposureProgram,            mdv)
      , ("Entfernung",           makerNotesFocusDistance,        mdv)
      , ("Tiefenschärfe",        compositeDOF,                   mdv)
      , ("Aufnahmemodus",        makerNotesShootingMode,         mdv)
      , ("Weißabgleich",         exifWhiteBalance,               mdv)
      , ("Aufnahmezähler",       makerNotesShutterCount,         mdv)
      , ("Geometrie",            compositeImageSize,             mdv)
      , ("Raw-Datei",            fileFileName,                   mdFile)
      , ("Bild-Datei",           fileRefJpg,                     mdv)
      , ("Bearbeitet",           fileFileModifyDate,             mdv)
      , ("Bewertung",            imgRating,                      mdRating)
      ]

-- ----------------------------------------

imgRef :: Text -> Text -> Text
imgRef theImgGeoDir theImgRef
  | isempty theImgRef = mempty
  | otherwise         = cond ("/" <>) theImgGeoDir <> theImgRef

imgRef' :: Text -> Text -> Text -> Text
imgRef' theImgGeoDir theIconGeoDir theIconRef
  | T.null theImgGeoDir = theIconRef
  | otherwise           = theIconGeoDir <> theIconRef

orgGeoDir :: Text
orgGeoDir = geoar'org ^. isoText

cond :: (Text -> Text) -> Text -> Text
cond f s
  | T.null s  = s
  | otherwise = f s

infixr 6 <:>

(<:>) :: Text -> Text -> Text
x <:> y
  | T.null y = x
  | otherwise = x <> ": " <> y

-- ----------------------------------------
