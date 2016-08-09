{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Catalog.Html.Templates.Photo2.AlbumPage where

import Catalog.Cmd.Types
import Data.Prim
import Data.String.QQ
import Text.SimpleTemplate

photo2Tmpl :: TmplEnv Cmd
photo2Tmpl =
  mempty
  & insSubTmpl "colPage"         colPage
  & insSubTmpl "colJS"           colJS
  & insSubTmpl "colTitle"        colTitle
  & insSubTmpl "colImg"          colImg
  & insSubTmpl "colNav"          colNav
  & insSubTmpl "colContents"     colContents
  & insSubTmpl "colRows"         colRows
  & insSubTmpl "colIcons"        colIcons
  & insSubTmpl "theNextImgRef"   theNextImgRef
  & insSubTmpl "thePrevImgRef"   thePrevImgRef
  & insSubTmpl "theChild1ImgRef" theChild1ImgRef
  & insSubTmpl "parentNav"       parentNav
  & insSubTmpl "prevNav"         prevNav
  & insSubTmpl "nextNav"         nextNav
  & insSubTmpl "child1Nav"       child1Nav
  & insSubTmpl "colBlog"         colBlog

  -- picture page templates
  & insSubTmpl "picPage"         picPage
  & insSubTmpl "picImg"          picImg
  & insSubTmpl "picTitle"        picTitle
  & insSubTmpl "picInfo"         picInfo
  & insSubTmpl "picNav"          picNav

  -- exif info
  & insSubTmpl "descrTitle"         descrTitle
  & insSubTmpl "descrSubtitle"      descrSubtitle
  & insSubTmpl "descrTitleEnglish"  descrTitleEnglish
  & insSubTmpl "descrTitleLatin"    descrTitleLatin
  & insSubTmpl "descrComment"       descrComment
  & insSubTmpl "descrWeb"           descrWeb
  & insSubTmpl "descrWikipedia"     descrWikipedia
  & insSubTmpl "descrGoogleMaps"    descrGoogleMaps

  & insSubTmpl "geoGPSLatitude"     geoGPSLatitude
  & insSubTmpl "geoGPSLongitude"    geoGPSLongitude
  & insSubTmpl "geoGPSAltitude"     geoGPSAltitude

  & insSubTmpl "exifCreateDate"     exifCreateDate
  & insSubTmpl "camCameraModelName" camCameraModelName
  & insSubTmpl "camLens"            camLens
  & insSubTmpl "camLensID"          camLensID
  & insSubTmpl "exifExposureTime"   exifExposureTime
  & insSubTmpl "exifExposureMode"   exifExposureMode
  & insSubTmpl "exifExposureProgram" exifExposureProgram
  & insSubTmpl "exifExposureCompensation" exifExposureCompensation
  & insSubTmpl "exifFNumber"        exifFNumber
  & insSubTmpl "exifFocusDistance"  exifFocusDistance
  & insSubTmpl "exifDepthOfField"   exifDepthOfField
  & insSubTmpl "exifISO"            exifISO
  & insSubTmpl "exifFocalLength"    exifFocalLength
  & insSubTmpl "exifFocalLengthIn35mmFormat" exifFocalLengthIn35mmFormat
  & insSubTmpl "exifShootingMode"   exifShootingMode
  & insSubTmpl "exifWhiteBalance"   exifWhiteBalance
  & insSubTmpl "exifImageSize"      exifImageSize
  & insSubTmpl "fileFileModificationDateTime" fileFileModificationDateTime
  & insSubTmpl "fileRefRaw"         fileRefRaw
  & insSubTmpl "fileRefJpg"         fileRefJpg

colPage :: Tmpl
colPage = parseTmpl [s|
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de">
  <head>
    <base href="${rootPath}"/>
    <title>${theHeadTitle}</title>
    <meta name="description"  content="Web Photo Album"/>
    <meta name="author"       content="Uwe Schmidt"/>
    <meta name="generator"    content="Photo Collection System"/>
    <meta name="date"         content="${theDate}"/>
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon"/>
    <link rel="stylesheet"    type="text/css" href="/assets/css/html-album.css"/>
${colJS}
    <script type="text/javascript" src="/assets/javascript/html-album.js" charset="ISO-8859-1">
    </script>
  </head>
  <body onload="initAlbum();"
        class="album album-${theImgGeo}"
        id="theAlbumBody">
    <table class="head">
      <tr>
        <td class="head1">
${colImg}
        </td>
        <td class="head2">
${colTitle}
        </td>
        <td class="head3">
${colNav}
        </td>
      </tr>
    </table>
${colBlog}
    <div class="ruler"></div>
    <div class="album-content">
${colContents}
    </div>
    <div class="ruler"></div>
    <table class="head">
      <tr>
        <td class="head1"></td>
        <td class="head2"></td>
        <td class="head3">
${colNav}
        </td>
      </tr>
    </table>
  </body>
</html>
|]

colBlog :: Tmpl
colBlog = parseTmpl [s|
    <div class="ruler"></div>
    <div class="blog-contents">
${blogContents}
    </div>
|]

colJS :: Tmpl
colJS = parseTmpl [s|
    <script type="text/javascript">
      <!--
      var duration = 7000 * ${theDuration};
      var thisp    = '${thisHref}';
      var thispos  = '${thisPos}'
      var nextp    = '${theNextHref}';
      var prevp    = '${thePrevHref}';
      var parentp  = '${theParentHref}';
      var childp   = '${theChild1Href}';
      var nextimg  = '${theNextImgRef}';
      var previmg  = '${thePrevImgRef}';
      -->
    </script>
|]

theNextImgRef :: Tmpl
theNextImgRef = parseTmpl "/${theImgGeoDir}${nextImgRef}"

thePrevImgRef :: Tmpl
thePrevImgRef = parseTmpl "/${theImgGeoDir}${prevImgRef}"

theChild1ImgRef :: Tmpl
theChild1ImgRef = parseTmpl "/${theImgGeo}${child1ImgRef}"

colTitle :: Tmpl
colTitle = parseTmpl [s|
          <div class="title">${theTitle}</div>
          <div class="subtitle">${theSubTitle}</div>
          <div class="comment">${theComment}</div>
|]

colImg :: Tmpl
colImg = parseTmpl [s|
          <img src="/${theIconGeoDir}${thisImgRef}"
               class="icon-${theIconGeoDir}"
               title="${theHeadTitle}"
               alt="${theHeadTitle}"/>
|]

col :: Tmpl
col = parseTmpl [s|
|]

colNav :: Tmpl
colNav = parseTmpl [s|
          <table class="nav" align="right">
            <tr>
              <td class="icon2">&nbsp;</td>
              <td class="icon2" id="theParentNav">
${parentNav}
              </td>
              <td class="icon2">&nbsp;</td>
            </tr>
            <tr>
              <td class="icon2" id="thePrevNav">
${prevNav}
              </td>
              <td class="icon2" id="theChild1Nav">
${child1Nav}
              </td>
              <td class="icon2" id="theNextNav">
${nextNav}
              </td>
            </tr>
          </table>
|]

parentNav :: Tmpl
parentNav = parseTmpl [s|
                <a href="javascript:parentPage();"
                   title="Album${theParentTitle}">
                  <img src="/${theIconGeoDir}${parentImgRef}"
                       class="icon2-${theIconGeoDir}"
                       alt="Album${theParentTitle}"/>
                </a>
|]

nextNav :: Tmpl
nextNav = parseTmpl [s|
                <a href="javascript:nextPage();"
                   title="weiter${theNextTitle}">
                  <img src="/${theIconGeoDir}${nextImgRef}"
                       class="icon2-${theIconGeoDir}"
                       alt="weiter${theNextTitle}"/>
                </a>
|]

prevNav :: Tmpl
prevNav = parseTmpl [s|
                <a href="javascript:prevPage();"
                   title="zur&uuml;ck${thePrevTitle}">
                  <img src="/${theIconGeoDir}${prevImgRef}"
                       class="icon2-${theIconGeoDir}"
                       alt="zur&uuml;ck${thePrevTitle}"/>
                </a>
|]

child1Nav :: Tmpl
child1Nav = parseTmpl [s|
                <a href="javascript:childPage(&apos;${theChild1Href}&apos;);"
                   title="1. Bild${theChild1Title}"
                   id="theChild1Nav">
                  <img src="/${theIconGeoDir}${child1ImgRef}"
                       class="icon2-${theIconGeoDir}"
                       alt="1. Bild${theChild1Title}"/>
                </a>
|]


colContents :: Tmpl
colContents = parseTmpl [s|
      <table class="col-contents">
${colRows}
      </table>
|]


colRows :: Tmpl
colRows = parseTmpl [s|
        <tr class="col-row">
${colIcons}
        </tr>
|]

colIcons :: Tmpl
colIcons = parseTmpl [s|
          <td class="icon-${theIconGeoDir}" id="${theChildId}" name="${theChildId}">
            <a href="javascript:childPage(&apos;${theChildHref}&apos;);"
               title="${theChildTitle}">
              <img src="/${theIconGeoDir}${theChildImgRef}"
                   class="icon-${theIconGeoDir}"
                   alt="${theChildTitle}"/>
            </a>
          </td>
|]

-- ----------------------------------------

picPage :: Tmpl
picPage = parseTmpl [s|
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de">
  <head>
    <base href="${rootPath}"/>
    <title>${theHeadTitle}</title>
    <meta name="description" content="Web Photo Album"/>
    <meta name="author"      content="Uwe Schmidt"/>
    <meta name="generator"   content="Photo Collection System"/>
    <meta name="date"        content="${theDate}"/>
    <link rel="stylesheet"   type="text/css" href="/assets/css/html-album.css"/>
${colJS}
    <script type="text/javascript" src="/assets/javascript/html-album.js" charset="ISO-8859-1"></script>
  </head>
  <body onload="initPicture();"
        class="picture picture-${theImgGeo}">
${picImg}
${picTitle}
${picInfo}
${picNav}
  </body>
</html>
|]

picImg :: Tmpl
picImg = parseTmpl [s|
    <div class="picture">
       <table class="picture">
         <tr>
           <td class="picture">
             <img src="/${theImgGeoDir}${thisImgRef}"
                  class="img-${theImgGeoDir}"/>
           </td>
         </tr>
       </table>
    </div>
|]

picTitle :: Tmpl
picTitle = parseTmpl [s|
    <div class="title-area" onmouseover="showTitle();" onmouseout="hideTitle();">
      <div class="title-area-line title-area-line-${theImgGeo}"
           id="title-area-line">
        <div class="title">${theTitle}</div>
        <div class="subtitle">${theSubTitle}</div>
        <div class="comment">${theComment}</div>
      </div>
    </div>
|]

picInfo :: Tmpl
picInfo = parseTmpl [s|
    <div class="info-area" onmouseover="showInfo();" onmouseout="hideInfo();">
      <div class="info-area-content info-area-content-${theImgGeo}"
           id="info-area-content">
        <div class="info">
          <div class="subtitle">Bild-Daten</div>
          <table class="info">
${descrTitle}
${descrSubtitle}
${descrTitleEnglish}
${descrTitleLatin}
${descrComment}
${descrWeb}
${descrWikipedia}
${descrGoogleMaps}
${geoGPSLatitude}
${geoGPSLongitude}
${geoGPSAltitude}
${exifCreateDate}
${camCameraModelName}
${camLens}
${camLensID}
${exifFocalLength}
${exifFocalLengthIn35mmFormat}
${exifExposureTime}
${exifExposureMode}
${exifExposureProgram}
${exifExposureCompensation}
${exifFNumber}
${exifFocusDistance}
${exifDepthOfField}
${exifISO}
${exifShootingMode}
${exifWhiteBalance}
${exifImageSize}
${fileRefRaw}
${fileRefJpg}
${fileFileModificationDateTime}
          </table>
        </div>
      </div>
    </div>
|]


descrTitle :: Tmpl
descrTitle = parseTmpl [s|
            <tr class="info" id="descr:Title">
              <th>Titel</th>
              <td>${descrTitleVal}</td>
            </tr>
|]

descrSubtitle :: Tmpl
descrSubtitle = parseTmpl [s|
            <tr class="info" id="descr:Subtitle">
              <th>Untertitel</th>
              <td>${descrSubtitleVal}</td>
            </tr>
|]

descrTitleEnglish :: Tmpl
descrTitleEnglish = parseTmpl [s|
            <tr class="info" id="descr:TitleEnglish">
              <th>Titel (engl.)</th>
              <td>${descrTitleEnglishVal}</td>
            </tr>
|]

descrTitleLatin :: Tmpl
descrTitleLatin = parseTmpl [s|
            <tr class="info" id="descr:TitleLatin">
              <th>Titel (lat.)</th>
              <td>${descrTitleLatinVal}</td>
            </tr>
|]

descrComment :: Tmpl
descrComment = parseTmpl [s|
            <tr class="info" id="descr:Comment">
              <th>Kommentar</th>
              <td>${descrCommentVal}</td>
            </tr>
|]

descrWeb :: Tmpl
descrWeb = parseTmpl [s|
            <tr class="info" id="descr:Web">
              <th>Web</th>
              <td><a href="${descrWebVal}">${descrWebVal}</a></td>
            </tr>
|]

descrWikipedia :: Tmpl
descrWikipedia = parseTmpl [s|
            <tr class="info" id="descr:Wikipedia">
              <th>Wikipedia</th>
              <td><a href="${descrWikipediaVal}">Wikipedia</a></td>
            </tr>
|]

descrGoogleMaps :: Tmpl
descrGoogleMaps = parseTmpl [s|
            <tr class="info" id="descr:GoogleMaps">
              <th>Karte</th>
              <td>
                <a href="https://maps.google.de/maps/@${locGoogleMaps},17z">${descrGoogleMapsVal}</a>
              </td>
            </tr>
|]

geoGPSLatitude :: Tmpl
geoGPSLatitude = parseTmpl [s|
            <tr class="info" id="geo:GPSLatitude">
              <th>Breitengrad</th>
              <td>${geoGPSLatitudeVal}</td>
            </tr>
|]

geoGPSLongitude :: Tmpl
geoGPSLongitude = parseTmpl [s|
            <tr class="info" id="geo:GPSLatitude">
              <th>L&auml;ngengrad</th>
              <td>${geoGPSLongitudeVal}</td>
            </tr>
|]

geoGPSAltitude :: Tmpl
geoGPSAltitude = parseTmpl [s|
            <tr class="info" id="geo:GPSAltitude">
              <th>H&ouml;he</th>
              <td>${geoGPSAltitudeVal}</td>
            </tr>
|]

exifCreateDate :: Tmpl
exifCreateDate = parseTmpl [s|
            <tr class="info" id="exif:CreateDate">
              <th>Aufnahmedatum</th>
              <td>${exifCreateDateVal}</td>
            </tr>
|]

camCameraModelName :: Tmpl
camCameraModelName = parseTmpl [s|
            <tr class="info" id="cam:CameraModelName">
              <th>Kamera</th>
              <td>${camCameraModelNameVal}</td>
            </tr>
|]

camLens :: Tmpl
camLens = parseTmpl [s|
            <tr class="info" id="cam:Lens">
              <th>Objektiv</th>
              <td>${camLensVal}</td>
            </tr>
|]

camLensID :: Tmpl
camLensID = parseTmpl [s|
            <tr class="info" id="cam:LensID">
              <th>Objektiv Typ</th>
              <td>${camLensIDVal}</td>
            </tr>
|]

exifExposureTime :: Tmpl
exifExposureTime = parseTmpl [s|
            <tr class="info" id="exif:ExposureTime">
              <th>Belichtungszeit</th>
              <td>${exifExposureTimeVal}</td>
            </tr>
|]

exifExposureMode :: Tmpl
exifExposureMode = parseTmpl [s|
            <tr class="info" id="exif:ExposureMode">
              <th>Belichtungsmessung</th>
              <td>${exifExposureModeVal}</td>
            </tr>
|]

exifExposureProgram :: Tmpl
exifExposureProgram = parseTmpl [s|
            <tr class="info" id="exif:ExposureProgram">
              <th>Aufnahmebetriebsart</th>
              <td>${exifExposureProgramVal}</td>
            </tr>
|]

exifExposureCompensation :: Tmpl
exifExposureCompensation = parseTmpl [s|
            <tr class="info" id="exif:ExposureCompensation">
              <th>Belichtungskorrektur</th>
              <td>${exifExposureCompensationVal}</td>
            </tr>
|]

exifFNumber :: Tmpl
exifFNumber = parseTmpl [s|
            <tr class="info" id="exif:FNumber">
              <th>Blende</th>
              <td>${exifFNumberVal}</td>
            </tr>
|]

exifFocusDistance :: Tmpl
exifFocusDistance = parseTmpl [s|
            <tr class="info" id="exif:FocusDistance">
              <th>Entfernung</th>
              <td>${exifFocusDistanceVal}</td>
            </tr>
|]

exifDepthOfField :: Tmpl
exifDepthOfField = parseTmpl [s|
            <tr class="info" id="exif:DepthOfField">
              <th>Tiefensch&auml;rfe</th>
              <td>${exifDepthOfFieldVal}</td>
            </tr>
|]

exifISO :: Tmpl
exifISO = parseTmpl [s|
            <tr class="info" id="exif:ISO">
              <th>ISO</th>
              <td>${exifISOVal}</td>
            </tr>
|]

exifFocalLength :: Tmpl
exifFocalLength = parseTmpl [s|
            <tr class="info" id="exif:FocalLength">
              <th>Brennweite</th>
              <td>${exifFocalLengthVal}</td>
            </tr>
|]

exifFocalLengthIn35mmFormat :: Tmpl
exifFocalLengthIn35mmFormat = parseTmpl [s|
            <tr class="info" id="exif:FocalLengthIn35mmFormat">
              <th>Brennweite in 35mm</th>
              <td>${exifFocalLengthIn35mmFormatVal}</td>
            </tr>
|]

exifShootingMode :: Tmpl
exifShootingMode = parseTmpl [s|
            <tr class="info" id="exif:ShootingMode">
              <th>Aufnahmemodus</th>
              <td>${exifShootingModeVal}</td>
            </tr>
|]

exifWhiteBalance :: Tmpl
exifWhiteBalance = parseTmpl [s|
            <tr class="info" id="exif:WhiteBalance">
              <th>Wei&szlig;abgleich</th>
              <td>${exifWhiteBalanceVal}</td>
            </tr>
|]

exifImageSize :: Tmpl
exifImageSize = parseTmpl [s|
            <tr class="info" id="exif:ImageSize">
              <th>Geometrie</th>
              <td>${exifImageSizeVal}</td>
            </tr>
|]

fileFileModificationDateTime :: Tmpl
fileFileModificationDateTime = parseTmpl [s|
            <tr class="info" id="file:FileModificationDateTime">
              <th>Bearbeitet</th>
              <td>${fileFileModificationDateTimeVal}</td>
            </tr>
|]

fileRefRaw :: Tmpl
fileRefRaw = parseTmpl [s|
            <tr class="info" id="file:RefRaw">
              <th>Raw-Datei</th>
              <td>${fileRefRawVal}</td>
            </tr>
|]

fileRefJpg :: Tmpl
fileRefJpg = parseTmpl [s|
            <tr class="info" id="file:RefJpg">
              <th>Bild-Datei</th>
              <td>${fileRefJpgVal}</td>
            </tr>
|]

picNav :: Tmpl
picNav = parseTmpl [s|
    <a href="javascript:parentPage()" title="Album" class="up" id="theUpButton">
      <span id="theContainingAlbum">&nbsp;</span>
    </a>
    <a href="javascript:prevPage()" title="zur&uuml;ck" class="back" id="theBackButton">
      <span id="thePreviousPic">&nbsp;</span>
    </a>
    <a href="javascript:nextPage()" title="weiter" class="forward" id="theForwardButton">
      <span id="theNextPic">&nbsp;</span>
    </a>
|]


-- ----------------------------------------
