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
  )
where

import           Data.MetaData -- (MetaData, metaDataAt, loc2googleMapsUrl)
import           Data.Monoid
import           Data.Prim
import qualified Data.Text as T
import           Text.Blaze.Html5 hiding (map, head)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes hiding (title, rows, accept)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html.Renderer.Pretty as R
import qualified Text.Blaze.Html.Renderer.Text   as T

renderPage' :: Html -> LazyText
renderPage' p = R.renderHtml p ^. isoText . lazy

renderPage :: Html -> LazyText
renderPage p = T.renderHtml p

t1 :: IO ()
t1 = putStr $ renderHtml $ p1

t2 :: IO ()
t2 = putStr $ renderHtml $ p2

t3 :: IO ()
t3 = putStr $ renderHtml $ p3

p1 :: Html
p1 = picPage'
  "A Picture"
  "today"
  "The Pic Title"
  "The Pic Subtitle"
  "A pic comment"
  (readGeo "1920x1200")
  "1.5"
  "/this/ref"
  "42"
  "/next/href"
  "/prev/href"
  "/parent/href"
  "pad-900x600"
  "/this/img.jpg"
  "/next/img.jpg"
  "/prev/img.jpg"
  mempty

p2 :: Html
p2 = colPage'
  "A Collection"
  "today"
  "The Title"
  "The Subtitle"
  "A comment"
  (readGeo "1920x1200")
  "1.0"
  "/this/ref"
  "43"
  "/next/href"
  "/prev/href"
  "/parent/href"
  "/child1/href"
  "pad-900x600"
  "fix-160x120"
  "/this/img.jpg"
  "/next/img.jpg"
  "/prev/img.jpg"
  "/child1/img.jpg"
  "the <em>blog text</em> is here"
  "The Parent Title"
  "/parent/img.jpg"
  "The next title"
  "the prev title"
  "the 1. image title"
  3
  (let l = ["abc", "123", "xyz", "789", "emil", "hilde", "otto"]
       l1 = map ("/href/" <>) l
       l2 = map ("/imgref/" <>) l
       l3 = map ("Title of " <>) l
       l4 = map (("id"<>) . T.pack . show) [1..]
   in
     zip4 l1 l2 l3 l4
  )

p3 :: Html
p3 = txtPage'
  "A Text page"
  "today"
  "5"
  "/this/ref"
  "44"
  "/next/href"
  "/prev/href"
  "/parent/href"
  "pad-900x600"
  "/next/img.jpg"
  "/prev/img.jpg"
  "The blog entry"

-- ----------------------------------------

type IconDescr = (Text, Text, Text, Text)

colPage' :: Text -> Text ->
            Text -> Text -> Text ->
            Geo  ->
            Text -> Text -> Text ->
            Text -> Text -> Text -> Text ->
            Text -> Text -> Text -> Text -> Text -> Text ->
            Text ->
            Text -> Text ->
            Text -> Text -> Text ->
            Int  -> [IconDescr] ->
            Html
colPage'
  theHeadTitle theDate
  theTitle theSubTitle theComment
  theImgGeo
  theDuration thisHref thisPos
  theNextHref thePrevHref theParentHref theChild1Href
  theImgGeoDir theIconGeoDir thisImgRef nextImgRef prevImgRef child1ImgRef
  cBlogContents
  theParentTitle parentImgRef
  theNextTitle thePrevTitle theChild1Title
  no'cols icons

  = colPage
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
      ( imgRef theImgGeoDir nextImgRef )
      ( imgRef theImgGeoDir prevImgRef )
    )
    ( colImg
      theIconGeoDir
      thisImgRef
      theHeadTitle
    )
    ( colTitle
      theTitle
      theSubTitle
      theComment
    )
    ( colNav
      ( parentNav theParentTitle parentImgRef               theIconGeoDir)
      ( prevNav   thePrevTitle   prevImgRef                 theIconGeoDir)
      ( child1Nav theChild1Title child1ImgRef theChild1Href theIconGeoDir)
      ( nextNav   theNextTitle   nextImgRef                 theIconGeoDir)
    )
    ( colBlog
      cBlogContents
    )
    ( colContents
      no'cols
      ( map (colIcon theIconGeoDir) icons )
    )

picPage' :: Text -> Text ->
            Text -> Text -> Text ->
            Geo  ->
            Text -> Text -> Text ->
            Text -> Text -> Text ->
            Text -> Text -> Text -> Text ->
            MetaData ->
            Html
picPage'
  theHeadTitle theDate
  theTitle theSubTitle theComment
  theImgGeo
  theDuration thisHref thisPos
  theNextHref thePrevHref theParentHref
  theImgGeoDir thisImgRef nextImgRef prevImgRef
  metaData
  = picPage
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
      ""
      ( imgRef theImgGeoDir nextImgRef )
      ( imgRef theImgGeoDir prevImgRef )
    )
    ( picImg
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

txtPage' :: Text -> Text ->
            Text -> Text -> Text ->
            Text -> Text -> Text ->
            Text -> Text -> Text ->
            Text ->
            Html
txtPage'
  theHeadTitle theDate
  theDuration thisHref thisPos
  theNextHref thePrevHref theParentHref
  theImgGeoDir nextImgRef prevImgRef
  blogContents
  = txtPage
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
      ( imgRef theImgGeoDir nextImgRef )
      ( imgRef theImgGeoDir prevImgRef )
    )
    blogContents

-- ----------------------------------------

picPage :: Text -> Text ->
           Geo  ->
           Html -> Html -> Html -> Html -> Html ->
           Html
picPage theHeadTitle theDate theImgGeo
        jsCode picImg picTitle picNav picInfo
  = htmlPage
    ( headPage theHeadTitle theDate jsCode ) $ do
  body ! onload "initAlbum();"
    ! class_ (toValue $ "picture picture-" <> (theImgGeo ^. isoText)) $ do
    picImg
    picTitle
    picInfo
    picNav

-- ----------------------------------------

txtPage :: Text -> Text -> Html -> Text -> Html
txtPage theHeadTitle theDate jsCode blogContents
  = htmlPage
    ( headPage theHeadTitle theDate jsCode ) $ do
  body ! onload "initPicture();"
    ! class_ "text" $ preEscapedText blogContents

-- ----------------------------------------

colPage :: Text -> Text -> Geo ->
           Html -> Html -> Html -> Html -> Html -> Html ->
           Html
colPage theHeadTitle theDate theImgGeo
        jsCode colImg colTitle colNav colBlog colContents
  = htmlPage
    ( headPage theHeadTitle theDate jsCode ) $ do
  body ! onload "initAlbum();"
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

headPage :: Text -> Text -> Html -> Html
headPage theHeadTitle theDate theJS
  = H.head $ do
  title $ toHtml theHeadTitle
  meta ! name "description" ! content "Web Photo Album"
  meta ! name "author"      ! content "Uwe Schmidt"
  meta ! name "generator"   ! content "catalog-server"
  meta ! name "date"        ! content (toValue theDate)
  link ! rel   "shortcut icon"
    ! href  "/favicon.ico"
    ! type_ "image/x-icon"
  link ! rel   "stylesheet"
    ! type_ "text/css"
    ! href  "/assets/css/html-album.css"
  theJS
  script ! type_   "text/javascript"
    ! src     "/assets/javascript/html-album.js"
    ! charset "ISO-8859-1"
    $ mempty

htmlPage :: Html -> Html -> Html
htmlPage theHead theBody = do
  docType
  html ! lang "de" $ do
    theHead
    theBody

-- ----------------------------------------

colBlog :: Text -> Html
colBlog cBlogContents
  | T.null cBlogContents = mempty
  | otherwise = do
      H.div ! class_ "ruler" $ mempty
      H.div ! class_ "blog-contents" $ preEscapedText cBlogContents

jsCode :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text ->
          Html
jsCode theDuration thisHref thisPos
      theNextHref thePrevHref theParentHref
      theChild1Href theNextImgRef thePrevImgRef
  = do
  script ! type_ "text/javascript" $ preEscapedText $ T.unlines $
    [ ""
    , "<!--"
    , "var duration = 7000 * '" <> theDuration <> "';"
    , "var thisp    = '" <> thisHref <> "';"
    , "var thispos  = '#" <> thisPos <> "';"
    , "var nextp    = '" <> theNextHref   <> "';"
    , "var prevp    = '" <> thePrevHref   <> "';"
    , "var parentp  = '" <> theParentHref <> "';"
    , "var childp   = '" <> theChild1Href <> "';"
    , "var nextimg  = '" <> theNextImgRef <> "';"
    , "var previmg  = '" <> thePrevImgRef <> "';"
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

colImg :: Text -> Text -> Text -> Html
colImg theIconGeoDir thisImgRef theHeadTitle =
  img ! src     (toValue $ imgRef theIconGeoDir thisImgRef)
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

parentNav :: Text -> Text -> Text -> Html
parentNav theParentTitle parentImgRef theIconGeoDir =
  nav' "javascript:parentPage();"
       ("Album" <:> theParentTitle)
       parentImgRef
       theIconGeoDir

nextNav :: Text -> Text -> Text -> Html
nextNav theNextTitle nextImgRef theIconGeoDir =
  nav' "javascript:nextPage();"
       ("weiter" <:> theNextTitle)
       nextImgRef
       theIconGeoDir

prevNav :: Text -> Text -> Text -> Html
prevNav thePrevTitle prevImgRef theIconGeoDir =
  nav' "javascript:prevPage();"
       ("zur\252ck" <:> thePrevTitle)
       prevImgRef
       theIconGeoDir

child1Nav :: Text -> Text -> Text -> Text -> Html
child1Nav theChild1Title child1ImgRef theChild1Href theIconGeoDir =
  nav' ("javascript:childPage('" <> theChild1Href <> "');")
       ("1. Bild" <:> theChild1Title)
       child1ImgRef
       theIconGeoDir


nav' :: Text -> Text -> Text -> Text -> Html
nav' theHref theTitle theImgRef theIconGeoDir
  | T.null theImgRef = mempty
  | otherwise        = do
      a ! href (toValue theHref)
        ! A.title (toValue theTitle) $ do
        img ! src    (toValue $ imgRef theIconGeoDir theImgRef)
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

colIcon :: Text -> IconDescr -> Html
colIcon theIconGeoDir (theChildHref, theChildImgRef, theChildTitle, theChildId) = do
  td ! class_ (toValue $ "icon-" <> theIconGeoDir)
     ! A.id   (toValue theChildId)
     ! A.name (toValue theChildId) $ do
    H.a ! href    (toValue $ "javascript:childPage('" <> theChildHref <> "');")
        ! A.title (toValue theChildTitle) $ do
      img ! src    (toValue $ imgRef theIconGeoDir theChildImgRef)
          ! class_ (toValue $ "icon-" <> theIconGeoDir)
          ! alt    (toValue theChildTitle)

-- ----------------------------------------

picImg :: Text -> Text -> Html
picImg theImgGeoDir thisImgRef =
  H.div ! class_ "picture" $
    table ! class_ "picture" $
      tr $
        td ! class_ "picture" $
          img ! src (toValue $ imgRef theImgGeoDir thisImgRef)
              ! class_ (toValue $ "img-" <> theImgGeoDir)

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

    mdv v = toHtml v

    mdWeb v =
      H.a ! href (toValue v) $ toHtml v

    mdWiki v =
      H.a ! href (toValue v) $ "--> Wikipedia"

    mdMap :: Text -> Html
    mdMap pos =
      H.a ! href (toValue gmhref) $ toHtml $ formatDegree pos
      where
        gmhref, gmloc :: Text
        gmhref = "https://maps.google.de/maps/@" <> gmloc <> ",17z"
        gmloc = loc2googleMapsUrl (pos ^. isoString) ^. from isoMaybe . isoText

    mdLoc v = toHtml $ formatDegree v

    -- subst " deg" by degree char '\176'
    formatDegree :: Text -> Text
    formatDegree = sed (const "\176") " deg"

    mdFile v =
      toHtml $ d <> "/" <> v
      where
        d = md ^. metaDataAt "File:Directory"

    mdTab :: [(Text, Name, Text -> Html)]
    mdTab =
      [ ("Titel",                descrTitle,                     mdv)
      , ("Untertitel",           descrSubtitle,                  mdv)
      , ("Titel (engl.)",        descrTitleEnglish,              mdv)
      , ("Titel (lat.)",         descrTitleLatin,                mdv)
      , ("Kommentar",            descrComment,                   mdv )
      , ("Web",                  descrWeb,                       mdWeb )
      , ("Wikipedia",            descrWikipedia,                 mdWiki )
      , ("Karte",                "Composite:GPSPosition",        mdMap )
      , ("Breitengrad",          "XMP:GPSLatitude",              mdLoc)
      , ("Längengrad",           "XMP:GPSLongitude",             mdLoc)
                                 -- disabled, Lightroom delivers nonsense data
      , ("Höhe",                 "???:GPSAltitude",              mdv)
      , ("Aufnahmedatum",        "EXIF:CreateDate",              mdv)
      , ("Kamera",               "EXIF:Model",                   mdv)
      , ("Objektiv",             "Composite:Lens",               mdv)
      , ("Objektiv Typ",         "Composite:LensID",             mdv)
      , ("Brennweite",           "EXIF:FocalLength",             mdv)
      , ("Brennweite in 35mm",   "EXIF:FocalLengthIn35mmFormat", mdv)
      , ("Belichtungszeit",      "EXIF:ExposureTime",            mdv)
      , ("Blende",               "EXIF:FNumber",                 mdv)
      , ("Belichtungskorrektur", "EXIF:ExposureCompensation",    mdv)
      , ("ISO",                  "EXIF:ISO",                     mdv)
      , ("Belichtungsmessung",   "EXIF:ExposureMode",            mdv)
      , ("Aufnahmebetriebsart",  "EXIF:ExposureProgram",         mdv)
      , ("Entfernung",           "EXIF:FocusDistance",           mdv)
      , ("Tiefenschärfe",        "Composite:DOF",                mdv)
      , ("Aufnahmemodus",        "EXIF:ShootingMode",            mdv)
      , ("Weißabgleich",         "EXIF:WhiteBalance",            mdv)
      , ("Geometrie",            "Composite:ImageSize",          mdv)
      , ("Raw-Datei",            "File:FileName",                mdFile)
      , ("Bild-Datei",           "File:RefJpg",                  mdv)
      , ("Bearbeitet",           "File:FileModifyDate",          mdv)
      ]

-- ----------------------------------------

imgRef :: Text -> Text -> Text
imgRef theImgGeoDir theImgRef = "/" <> theImgGeoDir <> theImgRef

infixr 6 <:>

(<:>) :: Text -> Text -> Text
x <:> y
  | T.null y = x
  | otherwise = x <> ": " <> y

-- ----------------------------------------
