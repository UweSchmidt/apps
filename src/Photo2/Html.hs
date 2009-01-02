module Photo2.Html
where

import           Data.Atom
import		 Data.List
import           Data.Maybe
import qualified Data.Map as M

import           Photo2.ArchiveTypes
import           Photo2.FilePath
import           Photo2.ImageOperations
import           Photo2.Arrow

import           Text.XML.HXT.Arrow
import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

-- ------------------------------------------------------------
--
-- generate HTML pages

albumKey, picKey, tempKey, layoutKey	:: Atom

albumKey	= newAtom "album"
picKey		= newAtom "picture"
tempKey		= newAtom "template"
layoutKey       = newAtom "layout"

titleKey, subTitleKey, resourceKey, durationKey :: Atom

titleKey	= newAtom "descr:Title"
subTitleKey	= newAtom "descr:Subtitle"
resourceKey	= newAtom "descr:Resource"
durationKey	= newAtom "show:Duration"

defLayoutKey	:: Atom
defLayoutKey	= newAtom "html-1024x768"

genHtml			:: Bool -> String -> ConfigArrow AlbumTree ()
genHtml rec format conf p0
    = runAction ("prepare generate HTML pages in format " ++ show format' ++ " for " ++ showPath p0) $
      ( checkEntryLoaded
	>>>
	( genPages $<<<
	  ( constL (maybeToList . M.lookup format' . confLayouts $ conf)
	    >>>
	    ( this
	      &&&
	      ( arr layoutPages
		>>>
		( ( (arrL $ maybeToList . M.lookup albumKey)
		    >>>
		    (arrL $ maybeToList . M.lookup tempKey)
		    >>>
		    readTemplate "album"
		  )
		  &&&
		  ( (arrL $ maybeToList . M.lookup picKey)
		    >>>
		    (arrL $ maybeToList . M.lookup tempKey)
		    >>>
		    readTemplate "picture"
		  )
		)
	      )
	    )
	  )
	)
	>>>
	constA ()
      )
      `orElse`
      errMsg ("no layout spec found for format " ++ show format')
    where
    dictId = newAtom "exif-german"
    translateExif	= translate
			  . maybe [("xxx","xxx")] (map (show *** id) . M.toList)
			  . M.lookup dictId
			  . confDict
			  $ conf
    format'
	| null format	= maybe defLayoutKey newAtom . M.lookup layoutKey . confAttrs $ conf
	| otherwise	= newAtom format

    readTemplate pt	= runAction ("read template for " ++ pt ++ " page") $
			  runInLocalURIContext $
			  ( ( readFromDocument [ (a_validate, v_0)
					       , (a_parse_html,v_1)
					       , (a_preserve_comment, v_1)
					       , (a_remove_whitespace, v_1)
					       , (a_trace, v_0)
					       ]
			      >>>
			      documentStatusOk
			    )
			    `orElse`
			    (clearErrStatus >>> none)
			  )
			  
    genPages	:: Layout -> XmlTree -> XmlTree -> CmdArrow AlbumTree AlbumTree
    genPages layout aTemplate pTemplate
	= runAction ("HTML page generation") $
	  ( if layoutType layout == "html-css2"
	    then genAllPages p0
	    else errMsg ("format type must be " ++ show "html-css2")
	  )
	where
        getAbs		:: PathArrow AlbumTree b -> Path -> CmdArrow a b
	getAbs pa p	= get theAlbums >>> pa p

	genAllPages p
	    = withLocalAlbums $
	      perform ( ( if rec
			  then loadChildAlbums	p	-- processChildren checkEntryLoaded
			  else this
			)
			>>>
			getAbs getTree p
			>>>
			gen1Page p
			>>>
			( if rec
			  then getChildrenAndProcess getChildren genAllPages p
			  else this
			)
		      )
	gen1Page	:: PathArrow AlbumTree AlbumTree
	gen1Page p
	    = -- runAction ("HTML Page for " ++ showPath p) $
	      perform ( genSinglePage p $<
			( getNode
			  &&&
			  listA ( getChildren >>> checkEntryLoaded )
			  &&&
			  getAbs getRelatives p
			)
		      )

	genSinglePage	:: Path -> (Pic, ([AlbumTree], (Path, Path, Path))) -> CmdArrow AlbumTree XmlTree
	genSinglePage p (pic, (cs, (par, prv, nxt)))
	    = choiceA [ isAlbum :-> constA aTemplate
		      , this    :-> constA pTemplate
		      ]
	      >>>
	      processTopDownWithAttrl
	      ( choiceA
		[ hasName "base"                :-> none	-- we do not use the base element
		  						-- for addressing the other docs
		  						-- addAttr "href" (joinPath . map (const "..") $ p)
		, insertText "[theTitle]"       theTitle
		, insertText "[theSubTitle]"    theSubTitle
		, insertText "[theResources]"   theResources
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
		  )				:-> (genTable $< (getAttrValue "size" >>> arr read))

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

                , insertNavT "[theParentTitle]" par
                , insertNavT "[thePrevTitle]"   prv
                , insertNavT "[the1ChildTitle]" child1p
                , insertNavT "[theNextTitle]"   nxt

		, guardNavi "theParentNav"      par
		, guardNavi "thePrevNav"        prv
		, guardNavi "the1ChildNav"      (if isRoot' then [] else child1p)	-- no navi in root page
		, guardNavi "theNextNav"        nxt

		, this                          :-> this
		]
	      )
              >>>
	      writeHtmlPage
	    where
	    isRoot'		= null par
            pas			= picAttrs pic
	    child1p		= concat .
				  runLA ( ( arrL (take 1)
					    >>>
					    getPicId
					    >>>
					    arr ((p ++) . (:[]))
					  )
					  `withDefault` []
					) $ cs

            thePath		= joinPath p
	    theUpPath           = joinPath . map (const "..") $ p
            theNextPath         = joinPath nxt
            thePrevPath         = joinPath prv
            theParentPath       = joinPath par
	    the1ChildPath	= joinPath child1p

	    theTitle		= valOf        titleKey
	    theSubTitle		= valOf        subTitleKey
	    theResources	= valOf        resourceKey
	    theDuration         = valOf' "1.0" durationKey
	    theHeadTitle	= removeMarkup theTitle
            theHeadTitle'
		| null theHeadTitle	= "\160"
		| otherwise		= theHeadTitle

            removeMarkup	= stringTrim . concat . runLA (xread >>> deep isText >>> getText)

	    replace n o		= sed (const n) (escRE o)

            insertText temp ins = hasMark temp :-> ( (getText >>^ replace ins temp) >>> xread )
	    insertP    temp ins = hasMark temp :-> changeText (insertPath temp ins)

            insertDate temp     = hasMark temp :-> ( ((getText &&& getDate) >>^ uncurry repl) >>> xread)
				  where
				  getDate    = arrIO0 getTimeStamp
				  repl t ins = replace ins temp t

	    insertNavT temp p'	= hasMark temp :-> ( changeNav $< getTitleText p' )
				  where
				  changeNav ins = changeText (replace ins' temp)
						  where
						  ins'
						     | null ins	 = ins
						     | otherwise = ": " ++ ins
            insertPath pt p'	= sed ins pt''
		                  where
                                  pt'  = escRE pt
				  pt'' = "\\[.*" ++ pt' ++ ".*\\]"
				  ins
				      | null p'		= const ""
				      | otherwise	= tail >>> init >>> replace p' pt >>> relPath

            insertInfoItem item	= if null val
				  then none
				  else processTopDownWithAttrl
				       ( choiceA [ insertText ("[" ++ item ++ "]") (tr val)
						 , this :-> this
						 ]
				       )
		                  where
				  val = valOf . newAtom $ item
				  tr	| "exif:" `isPrefixOf` item
					  ||
					  "cam:"  `isPrefixOf` item	= translateExif
					| otherwise			= id

            insertChild		:: Int -> AlbumTree -> CmdArrow XmlTree XmlTree
	    insertChild no t	= processTopDownWithAttrl
				  ( choiceA
				    [ insertP    "[theChildPath]"  (joinPath cp)
				    , let temp = "[theChildTitle]"
				      in hasMark temp :-> changeText (replace thePlainT temp)
				    , this :-> this
				    ]
				  )
	                          where
				  cp		= p ++ [thePicId]
				  thePicId	= picId thePic
				  thePic	= head . runLA getNode $ t
				  theTitleT	= getPlainTitle thePic

				  thePlainT	| not (null theTitleT)	= bno ++ ": " ++ theTitleT
						| match "pic-[0-9]+" thePicId
								       	= bno
									  -- sed (const "Bild ") "pic-0*" thePicId
						| otherwise		= bno ++ ": " ++ thePicId
						where
						bno			= ( if isAl thePic
									    then "Album "
									    else "Bild "
									  )
									  ++ show no

            genTable		:: Int -> CmdArrow XmlTree XmlTree
	    genTable n		= ( ( getChildren
				      >>>
				      hasName "td"
				      >>>
				      catA (zipWith insertChild [1..] cs)
				    )
				    >>.
				    part
				  )
				  >>>
				  selem "tr" [unlistA]
				  where
				  part []	= []
				  part l	= h : part r
						  where
						  (h, r) = splitAt n l

	    guardNavi nid p'	= hasId nid :-> ( ( if null p'
			                            then replaceChildren (txt "\160")
						    else this
						  )
                                                  >>>
						  removeAttr "id"
						)
            getTitleText p'	= getAbs getTree p'
				  >>>
				  getNode
				  >>>
				  arr getPlainTitle

            getPlainTitle	= picAttrs
				  >>>
				  M.lookup titleKey
				  >>>
				  fromMaybe ""
				  >>>
				  removeMarkup

            hasMark mark        = hasText (grep . escRE $ mark)
                                  where
				  anyCharSeq = "(.|[\n\r])*"
                                  grep re = match (anyCharSeq ++ re ++ anyCharSeq)

	    hasId n 		= hasAttrValue "id"    (==n)
	    hasClass n		= hasAttrValue "class" (==n)

	    valOf		= valOf' ""
            valOf' d n          = fromMaybe d . M.lookup n $ pas

            fullPath            = (show format' </>)
            relPath             = (theUpPath </>)

	    dst			= fullPath $ thePath `addExtension` "html"

            eeToHtml		= processBottomUp
				  ( replaceChildren (cmt " firefox hack ")	-- insert a comment for preventing e.g. <div/>
				    `when`
				    ( isElem
				      >>>
				      hasNameWith (localPart
						   >>>
						   (`notElem` ["meta", "link", "hr", "br", "img"])
						  )
				      >>>
				      neg getChildren
				    )
				  )

	    writeHtmlPage	:: CmdArrow XmlTree XmlTree
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
		  eeToHtml	-- this is a hack for firefox, it does not process empty xhtml elements, e.g. <div/>
		  >>>
		  writeDocument [ (a_indent, v_0)
				, (a_output_encoding, usAscii)
				] dst
		  >>>
		  traceStatus' ("written : " ++ show dst)

-- ------------------------------------------------------------

processTemplate	:: (ArrowTree a, Tree t, ArrowIO a) =>
		   [IfThen (a (t b) c) (a (t b) (t b))] ->
		   a (t b) (t b)

processTemplate cases
    = ( (choiceA cases `orElse` this)
	>>>
	processChildren (processTemplate cases)
      )

-- ------------------------------------------------------------

escRE		:: String -> String
escRE		= concatMap esc
		  where
		  esc c
		      | c `elem` "{}[].*+?|()\\"	= '\\' : c : []
		      | otherwise			= c : []

translate	:: AssocList String String -> String -> String
translate tb	= sed ( fromJust . (flip lookup) tb ) re
		  where
		  re = intercalate "|" . map (("("++) . (++")") . escRE . fst) $ tb

-- ------------------------------------------------------------
