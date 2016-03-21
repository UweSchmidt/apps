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

colPage :: Tmpl
colPage = parseTmpl [s|
<?xml version="1.0" encoding="ISO-8859-1"?>
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
    <script type="text/javascript" src="/assets/javascript/html-album.js" charset="ISO-8859-1"/>
  </head>
  <body onload="initAlbum();"
        class="album"
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
    <div class="ruler"/>
    <div class="album-content">
${colContents}
    </div>
    <div class="ruler"/>
    <table class="head">
      <tr>
        <td class="head1"/>
        <td class="head2"/>
        <td class="head3">
${colNav}
        </td>
      </tr>
    </table>
  </body>
</html>
|]

colJS :: Tmpl
colJS = parseTmpl [s|
    <script type="text/javascript">
      <!--
      var duration = 7000 * ${theDuration};
      var thisp    = '${thisHref}';
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
theNextImgRef = parseTmpl "/${theImgGeo}${nextImgRef}"

thePrevImgRef :: Tmpl
thePrevImgRef = parseTmpl "/${theImgGeo}${prevImgRef}"

theChild1ImgRef :: Tmpl
theChild1ImgRef = parseTmpl "/${theImgGeo}${child1ImgRef}"

colTitle :: Tmpl
colTitle = parseTmpl [s|
          <div class="title">${theTitle}</div>
          <div class="subtitle">${theSubTitle}</div>
          <div class="resources">${theResource}</div>
|]

colImg :: Tmpl
colImg = parseTmpl [s|
          <img src="/${theIconGeo}${thisImgRef}"
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
                  <img src="/${theIconGeo}${parentImgRef}"
                       class="icon2-${theIconGeoDir}"
                       alt="Album${theParentTitle}"/>
                </a>
|]

nextNav :: Tmpl
nextNav = parseTmpl [s|
                <a href="javascript:nextPage();"
                   title="weiter${theNextTitle}">
                  <img src="/${theIconGeo}${nextImgRef}"
                       class="icon2-${theIconGeoDir}"
                       alt="weiter${theNextTitle}"/>
                </a>
|]

prevNav :: Tmpl
prevNav = parseTmpl [s|
                <a href="javascript:prevPage();"
                   title="zur&uuml;ck${thePrevTitle}">
                  <img src="/${theIconGeo}${prevImgRef}"
                       class="icon2-${theIconGeoDir}"
                       alt="zur&uuml;ck${thePrevTitle}"/>
                </a>
|]

child1Nav :: Tmpl
child1Nav = parseTmpl [s|
                <a href="javascript:childPage(&apos;${theChild1Href}&apos;);"
                   title="1. Bild${theChild1Title}"
                   id="theChild1Nav">
                  <img src="/${theIconGeo}${child1ImgRef}"
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
          <td class="icon-${theIconGeoDir}" id="${theChildId}">
            <a href="javascript:childPage(&apos;${theChildHref}&apos;);"
               title="${theChildTitle}">
              <img src="/${theIconGeo}${theChildImgRef}"
                   class="icon-${theIconGeoDir}"
                   alt="${theChildTitle}"/>
            </a>
          </td>
|]

-- ----------------------------------------
