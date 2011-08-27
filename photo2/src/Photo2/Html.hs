module Photo2.Html
where

import           Data.Atom
import           Data.List
import           Data.Maybe
import qualified Data.Map as M

import           Photo2.ArchiveTypes
import           Photo2.ExifData
import           Photo2.FilePath
import           Photo2.ImageOperations
import           Photo2.Arrow

import           Text.XML.HXT.Core

import           Text.Regex.XMLSchema.String

-- ------------------------------------------------------------

albumKey, picKey, tempKey, layoutKey    :: Atom

albumKey        = newAtom "album"
picKey          = newAtom "picture"
tempKey         = newAtom "template"
layoutKey       = newAtom "layout"

defLayoutKey    :: Atom
defLayoutKey    = newAtom "html-1024x768"

-- ------------------------------------------------------------
--
-- generate HTML pages

genHtml                 :: Bool -> String -> ConfigArrow AlbumTree ()
genHtml rec formats conf p0
    = runAction ("prepare generate HTML pages in format(s) " ++ show formats' ++ " for " ++ showPath p0) $
      ( checkEntryLoaded
        >>>
        ( (flip genAllPages p0) $<
          listA ( constL formats'       -- (maybeToList . M.lookup format' . confLayouts $ conf)
                  >>>
                  ( this
                    &&&
                    ( ( arrL (\ f -> maybeToList . M.lookup (newAtom f) . confLayouts $ conf)
                        >>>
                        ( this
                          &&&
                          ( arr layoutPages
                            >>>
                            ( ( (arrL $ maybeToList . M.lookup albumKey)
                                >>>
                                (arrL $ maybeToList . M.lookup tempKey)
                                >>>
                                readTemplate
                              )
                              &&&
                              ( (arrL $ maybeToList . M.lookup picKey)
                                >>>
                                (arrL $ maybeToList . M.lookup tempKey)
                                >>>
                                readTemplate
                              )
                            )
                          )
                        )
                      )
                      `orElse`
                      errMsg ("layout spec not found")
                    )
                    >>>
                    filterCSS2
                  )
                )
        )
        >>>
        constA ()
      )
    where
    dictId              = newAtom "exif-german"
    translateExif       = translate
                          . maybe [("xxx","xxx")] (map (show *** id) . M.toList)
                          . M.lookup dictId
                          . confDict
                          $ conf
    translateGM         = ("<a href='" ++) . (++ "'>google maps</a>")
    translateWikipedia  = ("<a href='" ++) . (++ "'>Wikipedia</a>")
    translateWeb url    = ("<a href='" ++ url ++ "'>" ++ url ++ "</a>")
    translateWeb'       = ("<a href='" ++) . (++ "'>Web</a>")
    formats'            :: [String]
    formats'
        | null fs       = words . fromMaybe "html-1024x768" . M.lookup layoutKey . confAttrs $ conf
        | otherwise     = fs
        where
        fs = words formats

    filterCSS2          = ifP ((== "html-css2") . layoutType . fst . snd)
                              (arr (fst &&& snd . snd))
                              (errMsg ("layout type must be " ++ show "html-css2"))

    genAllPages         :: [(String, (XmlTree, XmlTree))] -> PathArrow AlbumTree AlbumTree
    genAllPages [] _pp
        = errMsg ("no layout for layout type \"html-css2\" found")
    genAllPages layouts pp
        = withLocalAlbums $
          perform ( getAbs getTree pp
                    >>>
                    processChildren checkEntryLoaded
                    >>>
                    seqA (map (flip gen1Page pp) layouts)
                    >>>
                    ( if rec
                      then getChildrenAndProcess getChildren (genAllPages layouts) pp
                      else this
                    )
                  )
        where
        getAbs          :: PathArrow AlbumTree b -> Path -> CmdArrow a b
        getAbs pa p     = get theAlbums >>> pa p

        gen1Page        :: (String, (XmlTree, XmlTree)) -> PathArrow AlbumTree AlbumTree
        gen1Page (f, (at, pt)) p
            = -- runAction ("HTML Page for " ++ showPath p ++ " and format " ++ show f) $
              perform ( genSinglePage f at pt p $<
                        ( getNode
                          &&&
                          listA ( getChildren >>> checkEntryLoaded )
                          &&&
                          getAbs getRelatives p
                        )
                      )

        genSinglePage   :: String -> XmlTree -> XmlTree ->
                           Path ->
                           (Pic, ([AlbumTree], (Path, Path, Path))) -> CmdArrow AlbumTree XmlTree
        genSinglePage format' aTemplate pTemplate p (pic, (cs, (par, prv, nxt)))
            = choiceA [ isAlbum :-> constA aTemplate
                      , this    :-> constA pTemplate
                      ]
              >>>
              processTopDownWithAttrl
              ( choiceA
                [ ( hasName "base"
                    >>>
                    hasAttrValue "href" (== "[rootPath]")
                  )                             :-> none

                , insertText "[theTitle]"       theTitle
                , insertText "[theSubTitle]"    theSubTitle
                , insertText "[theResources]"   theResources
                , insertText "[theGoogleMaps]"  theGoogleMaps
                , insertText "[theWeb]"         theWeb
                , insertText "[theWikipedia]"   theWikipedia
                , insertText "[theHeadTitle]"   theHeadTitle'
                , insertDate "[theDate]"

                , ( hasName "tr"
                    >>>
                    hasClass "info"
                  )                             :-> ( ( insertInfoItem $< getAttrValue "id" )
                                                      >>>
                                                      removeAttr "id"
                                                    )
                , ( hasName "tr"
                    >>>
                    hasId "theAlbumRow"
                  )                             :-> (genTable $< (getAttrValue "size" >>> arr read))

                                                    -- here the order is important
                , hasMark "[theJavaScriptCode]" :-> ( getText
                                                      >>>
                                                      arr ( replace ""          "[theJavaScriptCode]"
                                                            >>>
                                                            replace theDuration "[theDuration]"
                                                            >>>
                                                            insertPath "[theNextPath]"   theNextPath
                                                            >>>
                                                            insertPath "[thePrevPath]"   thePrevPath
                                                            >>>
                                                            insertPath "[theParentPath]" theParentPath
                                                            >>>
                                                            insertPath "[the1ChildPath]" the1ChildPath
                                                          )
                                                      >>>
                                                      mkCmt
                                                    )

                , insertText "[theUpPath]"      theUpPath

                , insertP    "[thePath]"        thePath
                , insertP    "[theParentPath]"  theParentPath
                , insertP    "[theNextPath]"    theNextPath
                , insertP    "[thePrevPath]"    thePrevPath
                , insertP    "[the1ChildPath]"  the1ChildPath

                , insertAP   "[theAbsPath]"        thePath
                , insertAP   "[theAbsParentPath]"  theParentPath
                , insertAP   "[theAbsNextPath]"    theNextPath
                , insertAP   "[theAbsPrevPath]"    thePrevPath
                , insertAP   "[theAbs1ChildPath]"  the1ChildPath

                , insertNavT "[theParentTitle]" par
                , insertNavT "[thePrevTitle]"   prv
                , insertNavT "[the1ChildTitle]" child1p
                , insertNavT "[theNextTitle]"   nxt

                , guardNavi "theParentNav"      par
                , guardNavi "thePrevNav"        prv
                , guardNavi "the1ChildNav"      (if isRoot' then [] else child1p)       -- no navi in root page
                , guardNavi "theNextNav"        nxt

                , this                          :-> this
                ]
              )
              >>>
              writeHtmlPage
            where
            isRoot'             = null par
            pas                 = picAttrs pic
            child1p             = concat .
                                  runLA ( ( arrL (take 1)
                                            >>>
                                            getPicId
                                            >>>
                                            arr ((p ++) . (:[]))
                                          )
                                          `withDefault` []
                                        ) $ cs

            thePath             = joinPath p
            theUpPath           = joinPath . map (const "..") $ p
            theNextPath         = joinPath nxt
            thePrevPath         = joinPath prv
            theParentPath       = joinPath par
            the1ChildPath       = joinPath child1p

            theTitle            = valOf        titleKey
            theSubTitle         = valOf        subTitleKey
            theResources        = valOf        resourceKey
            theGoogleMaps       = checkSet translateGM .
                                  valOf $      googleMapsKey
            theWeb              = checkSet translateWeb' .
                                  valOf $      webKey
            theWikipedia        = checkSet translateWikipedia .
                                  valOf $      wikipediaKey
            theDuration         = valOf' "1.0" durationKey
            theHeadTitle        = removeMarkup theTitle
            theHeadTitle'
                | null theHeadTitle     = "\160"
                | otherwise             = theHeadTitle

            checkSet _ ""       = ""
            checkSet f s        = f s

            removeMarkup        = stringTrim . concat . runLA (xread >>> deep isText >>> getText)

            replace n o         = sed (const n) (escRE o)

            insertText temp ins = hasMark temp :-> ( (getText >>^ replace ins temp) >>> xread )
            insertP    temp ins = hasMark temp :-> changeText (insertPath    temp ins)
            insertAP   temp ins = hasMark temp :-> changeText (insertAbsPath temp ins)

            insertDate temp     = hasMark temp :-> ( ((getText &&& getDate) >>^ uncurry repl) >>> xread)
                                  where
                                  getDate    = arrIO0 getTimeStamp
                                  repl t ins = replace ins temp t

            insertNavT temp p'  = hasMark temp :-> ( changeNav $< getTitleText p' )
                                  where
                                  changeNav ins = changeText (replace ins' temp)
                                                  where
                                                  ins'
                                                     | null ins  = ins
                                                     | otherwise = ": " ++ ins
            insertPath' rt pt p'= sed ins pt''
                                  where
                                  pt'  = escRE pt
                                  pt'' = "\\[.*" ++ pt' ++ ".*\\]"
                                  ins
                                      | null p'         = const ""
                                      | otherwise       = tail >>> init >>> replace p' pt >>> rt

            insertPath          = insertPath' relPath
            insertAbsPath       = insertPath' id

            insertInfoItem item = if null val
                                  then none
                                  else processTopDownWithAttrl
                                       ( choiceA [ insertText ("[" ++ item ++ "]") (tr val)
                                                 , this :-> this
                                                 ]
                                       )
                                  where
                                  val = valOf . newAtom $ item
                                  tr    | "exif:" `isPrefixOf` item
                                          ||
                                          "cam:"  `isPrefixOf` item     = translateExif
                                        | item == show googleMapsKey    = translateGM
                                        | item == show webKey           = translateWeb
                                        | item == show wikipediaKey     = translateWikipedia
                                        | otherwise                     = id

            insertChild         :: Int -> AlbumTree -> CmdArrow XmlTree XmlTree
            insertChild num t    = processTopDownWithAttrl
                                  ( choiceA
                                    [ insertP    "[theChildPath]"     (joinPath cp)
                                    , insertAP   "[theAbsChildPath]"  (joinPath cp)
                                    , let temp = "[theChildTitle]"
                                      in hasMark temp :-> changeText (replace thePlainT temp)
                                    , this :-> this
                                    ]
                                  )
                                  where
                                  cp            = p ++ [thePicId]
                                  thePicId      = picId thePic
                                  thePic        = head . runLA getNode $ t
                                  theTitleT     = getPlainTitle thePic

                                  thePlainT     | not (null theTitleT)  = bno ++ ": " ++ theTitleT
                                                | match "pic-[0-9]+" thePicId
                                                                        = bno
                                                                          -- sed (const "Bild ") "pic-0*" thePicId
                                                | otherwise             = bno ++ ": " ++ thePicId
                                                where
                                                bno                     = ( if isAl thePic
                                                                            then "Album "
                                                                            else "Bild "
                                                                          )
                                                                          ++ show num

            genTable            :: Int -> CmdArrow XmlTree XmlTree
            genTable n          = ( ( getChildren
                                      >>>
                                      hasName "td"
                                      >>>
                                      catA (zipWith insertChild nats cs)
                                    )
                                    >>.
                                    part
                                  )
                                  >>>
                                  selem "tr" [unlistA]
                                  where
                                  part []       = []
                                  part l        = h : part r
                                                  where
                                                  (h, r) = splitAt n l

            guardNavi nid p'    = hasId nid :-> ( ( if null p'
                                                    then replaceChildren (txt "\160")
                                                    else this
                                                  )
                                                  >>>
                                                  removeAttr "id"
                                                )
            getTitleText p'     = getAbs getTree p'
                                  >>>
                                  getNode
                                  >>>
                                  arr getPlainTitle

            getPlainTitle       = picAttrs
                                  >>>
                                  M.lookup titleKey
                                  >>>
                                  fromMaybe ""
                                  >>>
                                  removeMarkup

            hasMark mark        = hasText (grep' . escRE $ mark)
                                  where
                                  anyCharSeq = "(.|[\n\r])*"
                                  grep' re = match (anyCharSeq ++ re ++ anyCharSeq)

            hasId n             = hasAttrValue "id"    (==n)
            hasClass n          = hasAttrValue "class" (==n)

            valOf               = valOf' ""
            valOf' d n          = fromMaybe d . M.lookup n $ pas

            fullPath            = (format' </>)
            relPath             = (theUpPath </>)

            dst                 = fullPath $ thePath `addExtension` "html"

            writeHtmlPage       :: CmdArrow XmlTree XmlTree
            writeHtmlPage
                = {- runAction ("write HTML page to file " ++ show dst) $ -}
                  addDoctypeDecl "html"
                                 "-//W3C//DTD XHTML 1.0 Transitional//EN"
                                  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
                  >>>
                  perform (constA dst >>> arrIOE mkDirectoryPath)
                  >>>
                  indentDoc
                  >>>
                  this -- eeToHtml -- not longer neccessay because of added option withOutputXHTML
                  >>>
                  writeDocument [ withIndent yes
                                , withOutputHTML
                                , withOutputEncoding usAscii -- is better than utf8, works with all default encodings of all rotten browsers
                                ] dst
                  >>>
                  traceStatus' ("written : " ++ show dst)

-- ------------------------------------------------------------

processTemplate :: (ArrowTree a, Tree t, ArrowIO a) =>
                   [IfThen (a (t b) c) (a (t b) (t b))] ->
                   a (t b) (t b)

processTemplate cases
    = ( (choiceA cases `orElse` this)
        >>>
        processChildren (processTemplate cases)
      )

-- ------------------------------------------------------------

escRE           :: String -> String
escRE           = concatMap esc
                  where
                  esc c
                      | c `elem` "{}[].*+?|()\\"        = '\\' : c : []
                      | otherwise                       = c : []

translate       :: AssocList String String -> String -> String
translate tb    = sed ( fromJust . (flip lookup) tb ) re
                  where
                  re = intercalate "|" . map (("("++) . (++")") . escRE . fst) $ tb

-- ------------------------------------------------------------

readTemplate    :: CmdArrow String XmlTree
readTemplate
    = read' $< this
    where
    read' url
        = ( getUserState
            >>>
            arrL (load $ selTemplate url)
          )
          `orElse`
          ( cacheTemplate $< readTemplate' )
        where
        cacheTemplate   :: XmlTree -> CmdArrow String XmlTree
        cacheTemplate t
            = constA t
              >>>
              changeUserState (\ t' -> store (selTemplate url) [t'])
        readTemplate'   = runAction ("read template for " ++ show url) $
                          runInLocalURIContext $
                          ( ( readFromDocument [ withValidate no
                                               , withParseHTML yes
                                               , withPreserveComment yes
                                               , withRemoveWS yes
                                               , withTrace 0
                                               ]
                              >>>
                              documentStatusOk
                            )
                            `orElse`
                            (clearErrStatus >>> none)
                          )

-- ------------------------------------------------------------
