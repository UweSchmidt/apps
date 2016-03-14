{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Catalog.Html.Templates.Photo2.AlbumPage where

import Catalog.Cmd.Types
import Data.Prim
import Data.String.QQ
import Text.SimpleTemplate

photo2Tmpl :: EnvTmpl Cmd
photo2Tmpl =
  emptyEnvTmpl
  & insSubTmpl "colPage"     colPage
  & insSubTmpl "colJS"       colJS
  & insSubTmpl "colTitle"    colTitle
  & insSubTmpl "colImg"      colImg
  & insSubTmpl "colNav"      colNav
  & insSubTmpl "colContents" colContents
  & insSubTmpl "colRows"     colRows
  & insSubTmpl "colIcons"    colIcons

colPage :: Template
colPage = parseTemplate [s|
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
    <link rel="stylesheet"   type="text/css" href="${theUpPath}/config/html-1024x768.css"/>
    ${colJS}
    <script type="text/javascript" src="${theUpPath}/config/html-album.js" charset="ISO-8859-1"/>
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

colJS :: Template
colJS = parseTemplate [s|
    <script type="text/javascript">
      ${theJavaScriptCode}
      var duration = 7000 * ${theDuration};
      var nextp    = '${theNextPath}.html]';
      var prevp    = '${thePrevPath}.html]';
      var parentp  = '${theParentPath}.html]';
      var childp   = '${the1ChildPath}.html]';
      var nextimg  = '${theImgGeo}/${theNextPath}.jpg';
      var previmg  = '${theImgGeo}/${thePrevPath}.jpg';
      ${theJavaScriptCode}
    </script>
|]

colTitle :: Template
colTitle = parseTemplate [s|
          <div class="title">${theTitle}</div>
          <div class="subtitle">${theSubTitle}</div>
          <div class="resources">${theResources}</div>
|]

colImg :: Template
colImg = parseTemplate [s|
          <img src="[100x75/${thePath}.jpg]"
               class="icon"
               title="${theHeadTitle}"
               alt="${theHeadTitle}"/>
|]

col :: Template
col = parseTemplate [s|
|]

colNav :: Template
colNav = parseTemplate [s|
          <table class="nav" align="right">
            <tr>
              <td class="icon2">&nbsp;</td>
              <td class="icon2" id="theParentNav">
                <a href="javascript:parentPage();"
                   title="Album${theParentTitle}">
                  <img src="[100x75/${theParentPath}.jpg]"
                       width="50"
                       height="37"
                       class="icon2"
                       alt="Album${theParentTitle}"/>
                </a>
              </td>
              <td class="icon2">&nbsp;</td>
            </tr>
            <tr>
              <td class="icon2" id="thePrevNav">
                <a href="javascript:prevPage();"
                   title="zur&uuml;ck${thePrevTitle}">
                  <img src="[100x75/${thePrevPath}.jpg]"
                       width="50"
                       height="37"
                       class="icon2"
                       alt="zur&uuml;ck${thePrevTitle}"/>
                </a>
              </td>
              <td class="icon2" id="the1ChildNav">
                <a href="javascript:childPage(&apos;[html-1024x748/${the1ChildPath}.html]&apos;);"
                   title="1.Bild${the1ChildTitle}"
                   id="the1ChildNav">
                  <img src="[100x75/${the1ChildPath}.jpg]"
                       width="50"
                       height="37"
                       class="icon2"
                       alt="1.Bild${the1ChildPath}"/>
                </a>
              </td>
              <td class="icon2" id="theNextNav">
                <a href="javascript:nextPage();"
                   title="weiter${theNextTitle}">
                  <img src="[100x75/${theNextPath}.jpg]"
                       width="50"
                       height="37"
                       class="icon2"
                       alt="weiter${theNextTitle}"/>
                </a>
              </td>
            </tr>
          </table>
|]


colContents :: Template
colContents = parseTemplate [s|
      <table>
        ${colRows}
      </table>
|]


colRows :: Template
colRows = parseTemplate [s|
        <tr id="theAlbumRow" size="9">
          ${colIcons}
        </tr>
|]

colIcons :: Template
colIcons = parseTemplate [s|
          <td class="icon"
              id="theAlbumCell">
            <a href="javascript:childPage(&apos;${theChildPath}.html]&apos;);"
               title="${theChildTitle}">
              <img src="[100x75/${theChildPath}.jpg]"
                   width="100"
                   height="75"
                   class="icon"
                   alt="${theChildTitle}"/>
            </a>
          </td>
|]


-- ----------------------------------------

{- a very simple trest case }

foo :: Template
foo = parseTemplate [s|
Well here is a ${variable}
and another one ${apple}
in a pece of text
her is the ${bar}
here's an undefined macro ${xxx}
|]

bar :: Template
bar = parseTemplate [s|
content of template with name ${name}
|]

-- ----------------------------------------

e1 :: EnvTmpl IO
e1 = EnvTmpl $ M.fromList
  [ ("variable", txt' value)
  , ("apple", txt' orange)
  , ("bar", evalBar)
  , ("*", defAction)
  ]
  where
    evalBar :: Text -> EnvTmpl IO -> IO [Text]
    evalBar n (EnvTmpl env) = do
      putStrLn $ "inserting template: " ++ n ^.isoString
      evalTemplate bar env'
        where
          env' = EnvTmpl $ M.insert "name" (atxt (n ^. isoString)) env

    defAction :: Text -> EnvTmpl IO -> IO [Text]
    defAction n _env = do
      putStrLn $ "template variable not found: " ++ n ^. isoString
      return ["???"]

    value :: IO GeoAR
    value = do
      print (42::Int) >> return (GeoAR 1024 768 Pad)

    orange :: IO String
    orange = do
      print "inserting an orange" >> return "orange"

-- -}
-- ----------------------------------------
