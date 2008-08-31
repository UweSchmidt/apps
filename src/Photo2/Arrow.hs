module Photo2.Arrow
where

import           Control.Monad.Error ( runErrorT )

import           Data.Maybe
import qualified Data.Map as M

import           Photo2.ArchiveTypes
import           Photo2.Config
import           Photo2.ExifData
import           Photo2.FilePath
import           Photo2.ImageOperations

import           System.IO

import           Text.XML.HXT.Arrow
import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

-- ------------------------------------------------------------

type CmdArrow    a b =           IOStateArrow AppState a b
type PathArrow   a b = Path   -> CmdArrow  a b
type ConfigArrow a b = Config -> PathArrow a b

runCmd	:: CmdArrow a b -> AppState -> IO AppState
runCmd cmd s0
    = do
      (s1, _res) <- runIOSLA (setTraceLevel 0 >>> clear >>> cmd) (initialState s0) undefined
      return (xio_userState s1)

runCmd'	:: CmdArrow a b -> ([b] -> IO ()) -> AppState -> IO AppState
runCmd' cmd out
    = runCmd (listA cmd >>> arrIO out)

arrIOE	:: (a -> IOE b) -> CmdArrow a b
arrIOE io
    = arrIO (runErrorT . io)
      >>>
      ( ( arrIO (\ s -> hPutStrLn stderr s) >>> none )
	|||
	this
      )
	
-- ------------------------------------------------------------

setField	:: Setter AppState b -> CmdArrow b b
setField sf	= perform ( ( this &&& getUserState )
			    >>> arr (uncurry sf)
			    >>> setUserState
			  )

getField	:: Getter AppState b -> CmdArrow a b
getField gf	= getUserState >>^ gf

data SelArrow a b = SA { get :: CmdArrow a b
		       , set :: CmdArrow b b
		       }

mkSelA	:: Selector AppState b -> SelArrow a b
mkSelA (g, s) = SA { get = getField g
		   , set = setField s
		   }

theAlbums       :: SelArrow a AlbumTree
theAlbums	= mkSelA $ selAlbums

theConfig       :: SelArrow a Config
theConfig	= mkSelA $ selConfig

theConfigAttrs  :: SelArrow a Attrs
theConfigAttrs	= mkSelA $ selConfigAttrs

theConfigAttr  :: Name -> SelArrow a Value
theConfigAttr k	= mkSelA $ selConfigAttr k

theStatus       :: SelArrow a Status
theStatus	= mkSelA $ selStatus

theArchiveName  :: SelArrow a Href
theArchiveName	= mkSelA $ selArchiveName

theConfigName   :: SelArrow a Href
theConfigName	= mkSelA $ selConfigName

theWd 		:: SelArrow a Path
theWd           = mkSelA $ selWd

-- ------------------------------------------------------------

getConfig	:: (Config -> a) -> CmdArrow b a
getConfig cf	= get theConfig >>^ arr cf

-- ------------------------------------------------------------

updateNode'	:: ArrowTree a => (Pic -> Pic) -> a Pic Pic -> a AlbumTree AlbumTree
updateNode' setEdit update
    = ( ( setNode $< (getNode >>> update >>> arr setEdit) )
      )
      `orElse`
      this

updateNode	:: ArrowTree a => a Pic Pic -> a AlbumTree AlbumTree
updateNode	= updateNode' id

editNode	:: ArrowTree a => a Pic Pic -> a AlbumTree AlbumTree
editNode	= updateNode' (change theEdited (const True))

clearEdited	:: ArrowTree a => a AlbumTree AlbumTree
clearEdited	= updateNode' (change theEdited (const False)) this

-- ------------------------------------------------------------

setComp		:: SelArrow a b -> b -> CmdArrow a a
setComp c v	= perform $ constA v >>> set c

changeComp	:: SelArrow a b -> (b -> b) -> CmdArrow a a
changeComp c f	= perform $ get c >>> f ^>> set c

clear		:: CmdArrow a a
clear		= setComp theStatus (Running 0)

done		:: CmdArrow a a
done		= changeComp theStatus stopTr

start		:: CmdArrow a a
start		= changeComp theStatus startTr

failed		:: String -> CmdArrow a a
failed msg	= setComp theStatus (Exc msg)

setArchiveName	:: String -> CmdArrow a a
setArchiveName n = setComp theArchiveName n

statusOK	:: CmdArrow a Status
statusOK	= get theStatus >>> isA statusOk

traceStatus	:: String -> CmdArrow a a
traceStatus msg
    = perform $
      traceS $<< (get theStatus &&& get (theConfigAttr "debug"))
    where
    traceS st "1" = traceMsg 0 (replicate (level st) ' ' ++ msg)
    traceS _  _   = this

    level (Running i)	= 2 * i
    level _		= 0

-- ------------------------------------------------------------

withStatusCheck	:: String -> CmdArrow a b -> CmdArrow a b
withStatusCheck msg action
    = start
      >>>
      traceStatus ("starting: " ++ msg)
      >>>
      ( ( action
	  >>>
	  traceStatus ("done    : " ++ msg)
	  >>>
	  done
	)
	`orElse`
	( traceStatus ("failed  : " ++ msg)
	  >>>
	  failed msg
	  >>>
	 none
	)
      )

whenStatusOK	:: String -> CmdArrow a b -> CmdArrow a b
whenStatusOK msg action
    = ( statusOK `guards` ( runAction' $< get theStatus ) )
      `orElse`
      ( traceStatus ("error   : " ++ msg)
	>>>
	none
      )
    where
    runAction' oldStatus
	= action
	  >>>
	  setComp theStatus oldStatus

runAction	:: String -> CmdArrow a b -> CmdArrow a b
runAction msg action
    = whenStatusOK msg (withStatusCheck msg action)

-- ------------------------------------------------------------

loadDocData	:: PU b -> String -> CmdArrow a b
loadDocData p doc
    = readDocument [ (a_remove_whitespace, v_1)
		   , (a_validate, v_0)
		   , (a_tagsoup, v_1)
		   ] doc
      >>>
      documentStatusOk
      >>>
      runAction ("unpickle document: " ++ doc) (xunpickleVal p)
      >>>
      perform none -- ( xpickleDocument p [ (a_indent, v_1) ] "" )
      >>>
      traceStatus ("loaded  : " ++ show doc)

loadArchive	:: String -> CmdArrow a Archive
loadArchive doc
    = runAction ("loading and unpickling archive: " ++ show doc)
      ( loadDocData xpArchive doc
	>>>
	perform ( archRootAlbum ^>> set theAlbums )
	>>>
	perform ( archConfRef ^>> set theConfigName )
	>>>
	setArchiveName doc
      )

loadConfig	:: String -> CmdArrow a Config
loadConfig doc
    = runAction ("loading and unpickling config: " ++ show doc)
      ( runInLocalURIContext (loadDocData xpConfig doc)
	>>>
	set theConfig
      )

loadArchiveAndConfig	:: String -> CmdArrow a (AlbumTree, Config)
loadArchiveAndConfig doc
    = loadArchive doc
      >>>
      ( arr archRootAlbum
	&&&
	( loadConfig $< arr archConfRef )
      )

loadAlbum	:: String -> String -> CmdArrow a AlbumTree
loadAlbum base doc
    = runAction ("loading and unpickling album: " ++ show doc ++ " (base = " ++ show base ++ ")")
      ( runInLocalURIContext ( constA base >>> changeBaseURI
			       >>>
			       loadDocData xpAlbumTree doc
			     )
      )

loadAlbums	:: PathArrow a AlbumTree
loadAlbums p
    = runAction ("check loaded albums for " ++ showPath p) $
      get theAlbums
      >>>
      processAllNodesOnPath checkAlbum p
      >>>
      set theAlbums

loadAllAlbums	:: PathArrow a AlbumTree
loadAllAlbums p
    = get theAlbums
      >>>
      processAllNodesOnPath    checkAlbum p
      >>>
      processAllSubTreesByPath checkAlbum p
      >>>
      set theAlbums

-- ------------------------------------------------------------

-- store the album and all subalbums adressed by a path and mark them as unloaded

storeAllAlbums	:: PathArrow a AlbumTree
storeAllAlbums p
    = get theAlbums
      >>>
      processTreeByPath storeAlbumTree p
      >>>
      set theAlbums

storeAlbumTree	:: PathArrow AlbumTree AlbumTree
storeAlbumTree p
    = runAction ("storing all albums at: " ++ showPath p) $
      ( ( editNode this `when` (getChildren >>> entryEdited) ) -- propagate changes to parent
	>>>
	processChildren (storeSubAlbums $< getPicId)
	>>>
	( ( removeSubAlbums
	    >>>
	    ( ( storeAlbum $<< ( getConfig (albumPath p) &&& get theConfigName ) )
	      `when`
	      entryEdited
	    )
	    >>>
	    clearEdited
	  )
	  `orElse`
	  this	-- something went wrong
	)
      )
      `when`
      isAlbum
    where
    storeSubAlbums n
	= storeAlbumTree (p ++ [n])
	  `when`
	  getChildren
    removeSubAlbums 
	= processChildren (setChildren [])

storeAlbum	:: FilePath -> FilePath -> CmdArrow AlbumTree AlbumTree
storeAlbum doc config
    = storeDocData xpAlbumTree "album" doc config
      `guards`
      updateNode ( arr $ change theRef (const doc) )

storeDocData	:: PU b -> String -> FilePath -> FilePath -> CmdArrow b XmlTree
storeDocData p root doc config
    = runAction ("pickle document: " ++ doc)
                ( xpickleVal p ) 
      >>>
      runAction ("write document:  " ++ doc)
		( addDoctypeDecl root "" (pathFromTo doc (dirName config </> "archive.dtd"))
		  >>>
		  perform (constA doc >>> arrIOE (mkBackupFile ".bak"))
		  >>>
		  writeDocument [ (a_indent, v_1)
				, (a_output_encoding, isoLatin1)
				] doc
		)
      >>>
      documentStatusOk
      >>>
      traceStatus ("stored  : " ++ show doc)

isAlbum		:: CmdArrow AlbumTree AlbumTree
isAlbum		= ( getNode
		    >>>
		    isA isAl 
		  )
		  `guards` this

getExtAlbumRef	:: CmdArrow AlbumTree String
getExtAlbumRef	= getNode
		  >>>
		  picRef
		  ^>>
		  isA (not . null)

entryEdited	:: CmdArrow AlbumTree AlbumTree
entryEdited 	= ( getNode >>> isA picEdited )
		  `guards`
		  this

storeConfig	:: CmdArrow b XmlTree
storeConfig
    = store $< get theConfigName
    where
    store doc
	= runAction ("storing configuration: " ++ doc)
	            ( get theConfig
		      >>>
		      storeDocData xpConfig "config" doc doc
		    )

storeArchive	:: CmdArrow b XmlTree
storeArchive
    = store $<< (get theArchiveName &&& get theConfigName)
    where
    store doc configName
	= runAction ("storing the archive: " ++ doc)
	  ( get theAlbums
	    >>>
	    withRootDir cut
	    >>>
	    arr (Archive configName)
	    >>>
	    storeDocData xpArchive "archive" doc configName
	  )
    cut p
	= setChildren []
	  >>>
	  ( (\ doc -> updateNode (arr $ change theRef (const doc))) $< getConfig (albumPath p) )

-- ------------------------------------------------------------
--
-- check whether an album is loaded, if not yet done, load the album

checkAlbum	:: PathArrow AlbumTree AlbumTree
checkAlbum _p
    = ( getExtAlbumRef
	>>>
	( loadAlbum $<< get theArchiveName &&& this )
      )
      `orElse` this

-- check whether an entry addresed by a path exists

checkPath	:: PathArrow AlbumTree AlbumTree
checkPath p
    = runAction ("checking path: " ++ showPath p) $
      getTreeByPath p

-- ------------------------------------------------------------

getTreeByPath	:: PathArrow AlbumTree AlbumTree
getTreeByPath	= getTreeByPathAndProcess (const this)

getTreeByPathAndProcess	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeByPathAndProcess pa
    = getTreeByPathAndProcess' []
    where
    getTreeByPathAndProcess' rp p
	| null p	= none
	| null p'	= nodeMatch `guards` pa (reverse rp')
	| otherwise = nodeMatch `guards` (getChildren >>> getTreeByPathAndProcess' rp' p')
	where
	(n' : p') = p
	rp' = n' : rp
	nodeMatch = hasPicName n'

getTreeByPathAndProcessChildren	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeByPathAndProcessChildren = getTreeByPathAndProcess . getChildrenAndProcess

getTreeByPathAndProcessDesc	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeByPathAndProcessDesc	 = getTreeByPathAndProcess . getDescAndProcess

getTreeByPathAndProcessSelfAndDesc	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeByPathAndProcessSelfAndDesc	 = getTreeByPathAndProcess . getSelfAndDescAndProcess

getChildrenAndProcess	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getChildrenAndProcess pa p
    = getChildren
      >>>
      ( pa $< (getPicId >>^ (reverse . (:rp))) )
    where
    rp = reverse p	-- make p ++ [n] a bit more efficient

getDescAndProcess	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getDescAndProcess pa p
    = getD (reverse p)	-- make p ++ [n] a bit more efficient
    where
    getD rp = getChildren
	      >>>
	      ( (\ rp' -> pa (reverse rp') <+> getD rp') $< (getPicId >>^ (:rp)) )

getSelfAndDescAndProcess	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getSelfAndDescAndProcess pa p
    = pa p
      <+>
      getDescAndProcess pa p

getDescByPath	:: PathArrow AlbumTree AlbumTree
getDescByPath p
    | null p	= this
    | otherwise = nodeMatch `guards` (getChildren >>> getDescByPath p')
    where
    (n' : p') = p
    nodeMatch = hasPicName n'
{-
processTreeByPath	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeByPath pa p
    | null p	= this
    | null p'	= pa p `when` hasPicName n'
    | otherwise	= processChildren (processTreeByPath pa p')
    where
    (n' : p') = p
-}
processAllNodesOnPath	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processAllNodesOnPath pa p
    | null p	= this
    | null p'	= pa p `when` hasPicName n'
    | otherwise	= ( pa p >>> processChildren (processAllNodesOnPath pa p') ) `when` hasPicName n'
    where
    (n' : p') = p

-- ----------------------------------------
--
-- | process all nodes of a tree

processAllByPath	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processAllByPath pa p
    = pa p
      >>>
      processChildren ( (\ n -> processAllByPath pa (p ++ [n])) $< getPicId )

-- ----------------------------------------
--
-- | process all nodes of a tree addressed by a path
--   with an arrow getting the full path as parameter

processAllSubTreesByPath	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processAllSubTreesByPath pa p0
    = processSub p0
    where
    processSub p
	| null p	= this
	| null p'	= processAllByPath pa p0          `when` hasPicName n'
	| otherwise	= processChildren (processSub p') `when` hasPicName n'
	where
	(n' : p') = p

-- ----------------------------------------
--
-- | process a tree addressed by a path
--   with an arrow getting the full path as parameter

processTreeByPath	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeByPath pa p0
    = processSub p0
    where
    processSub p
	| null p	= this
	| null p'	= pa p0                           `when` hasPicName n'
	| otherwise	= processChildren (processSub p') `when` hasPicName n'
	where
	(n' : p') = p

-- ----------------------------------------

hasPicName	:: Name -> CmdArrow AlbumTree AlbumTree
hasPicName n    = (getPicId >>> isA (==n)) `guards` this

getPicId	:: CmdArrow AlbumTree Name
getPicId	= getNode >>^ picId

mkPic		:: Pic -> CmdArrow AlbumTree AlbumTree
mkPic		= mkLeaf

getRootPath	:: CmdArrow a Path
getRootPath	= get theAlbums >>> getPicId >>^ (:[])

rootWd		:: CmdArrow a Path
rootWd		= getRootPath >>> set theWd

withDir		:: Path -> PathArrow a b -> CmdArrow a b
withDir p c	= (\ p' -> withAbsDir p' c) $< (get theWd >>^ (++ p))

withAbsDir	:: Path -> PathArrow a b -> CmdArrow a b
withAbsDir p c	= c (normalPath p)

withCwd		:: PathArrow a b -> CmdArrow a b
withCwd		= withDir []

withRootDir	:: PathArrow a b -> CmdArrow a b
withRootDir c	= (\ p' -> withAbsDir p' c) $< getRootPath

mkAbs		:: CmdArrow Path Path
mkAbs		= (\ wd -> arr (wd ++)) $< get theWd

withConfig	:: ConfigArrow a b -> PathArrow a b
withConfig c p	= ( \ config -> c config p ) $< get theConfig

-- ----------------------------------------

getRelatives	:: PathArrow AlbumTree (Path, Path, Path)
getRelatives p
    = runAction ("get relatives: " ++ showPath p) $
      getPN ( if null p then emptyPath else init p )
    where
    n = last p
    getPN pp
	= ( listA ( getTreeByPath pp
		    >>>
		    getChildren
		    >>>
		    getNode
		    >>^
		    picId
		  )
	    >>>
	    ( ( arrL getp `orElse` constA emptyPath )
	      &&&
	      ( arrL getn `orElse` constA emptyPath )
	    )
            >>^ (\ (p', n') -> (pp, p',n'))
	  )
          `orElse`
	  constA (pp, emptyPath, emptyPath)
	where
	getn :: [Name] -> [Path]
	getn l = take 1 . map ((pp ++) . (:[]) . snd) . filter ((== n) . fst) . zip l . drop 1 $ l

	getp :: [Name] -> [Path]
	getp l = take 1 . map ((pp ++) . (:[]) . snd) . filter ((== n) . fst) . zip (drop 1 l) $ l

getAlbumEntry	:: PathArrow AlbumTree AlbumEntry
getAlbumEntry p
    | null p	= none
    | otherwise	= getDescByPath p'
		  >>>
		  hasPicName n'
		  >>>
		  getNode
		  >>^
		  (\ x -> (p, x))
    where
    n' = last p
    p' = init p

getAlbumPaths		:: PathArrow AlbumTree Path
getAlbumPaths		= getTreeByPathAndProcessChildren constA

getAllAlbumPaths	:: PathArrow AlbumTree Path
getAllAlbumPaths	= getTreeByPathAndProcessDesc constA

getAllEditedPaths	:: PathArrow AlbumTree Path
getAllEditedPaths	= getTreeByPathAndProcessDesc $
			  \ p -> entryEdited `guards` constA p

getAllWithAttr		:: String -> String -> PathArrow AlbumTree (Path, String, String)
getAllWithAttr rek rev
    = getTreeByPathAndProcessSelfAndDesc $
      \ p -> getNode
	     >>>
	     arrL (load theAttrs >>> M.toList)
	     >>>
	     ( if null rek
	       then this
	       else isA (fromMaybe False . matchRE rek . fst)
	     )
             >>>
	     ( if null rev
	       then this
	       else isA (fromMaybe False . matchRE rev . snd)
	     )
	     >>>
	     arr (\ (k, v) -> (p, k, v))

-- ------------------------------------------------------------

addAlbumEntry	:: AlbumEntry -> CmdArrow AlbumTree AlbumTree
addAlbumEntry (p0, pic)
    = insert p0
    where
    n = picId pic
    insert	:: PathArrow AlbumTree AlbumTree
    insert p
	| null p	= setNode pic `when` hasPicName n		-- entry already in tree
	| null p'	= replaceChildren (getChildren <+> mkPic pic)	-- append a new leave to the children
			  `when`
			  hasPicName n'
	| otherwise	= processChildren (insert p')			-- descend into tree
			  `when`
			  hasPicName n'
	where
	(n'  : p' ) = p

removeAlbumEntry	:: PathArrow AlbumTree AlbumTree
removeAlbumEntry p
    = processTreeByPath (const none) p

-- ------------------------------------------------------------
-- update an attribute for an album or picture

updateAttrKeys	:: PathArrow AlbumTree AlbumTree
updateAttrKeys p
    = runAction ("updating attribute keys for " ++ showPath p)
      ( checkAlbum p
	>>>
	editNode (arr $ change theAttrs normAttrs)
      )

updateAttr	:: String -> String -> PathArrow AlbumTree AlbumTree
updateAttr an av p
    = runAction ("updating " ++ showPath p ++ " attr " ++ show an ++ " with value " ++ show av)
      ( checkAlbum p
	>>>
	editNode (arr $ change theAttrs (mergeAttr an av))
      )

updateAttrs	:: Attrs -> PathArrow AlbumTree AlbumTree
updateAttrs am p
    = runAction ("updating " ++ showPath p ++ " attributes " ++ show am)
      ( checkAlbum p
	>>>
	editNode (arr $ change theAttrs (mergeAttrs am))
      )

updateExifAttrs	:: ConfigArrow AlbumTree AlbumTree
updateExifAttrs c p
    = runAction ("updating " ++ showPath p ++ " exif attributes")
      ( checkAlbum p
	>>>
	editNode (arrIOE (importExifAttrs c p))
      )

-- ------------------------------------------------------------
--
-- rename picture

renamePic	:: Name -> ConfigArrow AlbumTree AlbumTree
renamePic nn c p
    = runAction ("renaming " ++ showPath p ++ " to " ++ nn)
      ( checkAlbum p
	>>>
	editNode (arrIOE (mvPic nn c p))
      )

renameContent	:: ConfigArrow AlbumTree AlbumTree
renameContent c p
    = runAction ("renaming album contents for " ++ showPath p)
      ( checkAlbum p
	>>>
	( rename $< listA (getChildren >>> (getNode >>^ picId)) )
      )
    where
    rename	:: [Name] -> CmdArrow AlbumTree AlbumTree
    rename ids
	| null nameMap
	    = this
	| otherwise		-- rename in 2 steps, to prevent name clashes
	    = processChildren (renameChild nameMap1 $< (getNode >>^ picId))
	      >>>
	      processChildren (renameChild nameMap2 $< (getNode >>^ picId))
	where
	renameChild nm cid
	    | isNothing nid	= this
	    | otherwise         = renamePic (fromJust nid) c (p ++ [cid])
				  `whenNot`
				  isAlbum
	    where
	    nid = lookup cid nm

	nameMap		:: [(Name, Name)]
	nameMap		= filter isNewName (zip ids (map picnr [1..]))

        nameMap1	= map (\ (o, n) -> (o, "#" ++ n)) $ nameMap
        nameMap2	= map (\ (o, n) -> ("#" ++ n, n)) $ nameMap

        isNewName	:: (Name, Name) -> Bool
	isNewName (o, n)
	    = n /= o
	      &&
	      (fromMaybe False . matchRE "pic-[0-9]+" $ o)

	picnr :: Int -> String
	picnr = show
		>>> reverse
		>>> (++ "0000")
		>>> take 4
		>>> reverse
		>>> ("pic-" ++)

-- ------------------------------------------------------------
--
-- update image data for a single node, the arrow input

updatePic	:: ConfigArrow AlbumTree AlbumTree
updatePic c p
    = runAction ("updating " ++ p')
      ( checkAlbum p
	>>>
	editNode update
      )
    where
    p' = showPath p
    sl = confSizes c
    update
	= runAction ("import original for " ++ p')
	  ( arrIOE (importOrig c p) )
	  >>>
	  seqA (map copy sl)
    copy s
	= runAction ("create copy for " ++ show (sizeDir s) ++ " for " ++ p')
	  ( arrIOE (createCopy c p s) )

-- update all entries addresed by a path

updateAllPics	:: PathArrow AlbumTree AlbumTree
updateAllPics
    = processTreeByPath
      ( processAllByPath
	( \ p -> ( withConfig updatePic p
		   >>>
		   withConfig updateExifAttrs p
		 )
	)
      )

updateAllAttrKeys	:: PathArrow AlbumTree AlbumTree
updateAllAttrKeys
    = processTreeByPath
      ( processAllByPath updateAttrKeys )

-- ------------------------------------------------------------
