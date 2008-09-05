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

infixr 1 />>>/

-- lift >>> to PathArrow level

(/>>>/) 	:: PathArrow b c -> PathArrow c d -> PathArrow b d
(/>>>/) f g	= \ p -> f p >>> g p


withDefaultRes	:: PathArrow b c -> c -> PathArrow b c
withDefaultRes f d
		= \ p -> f p `withDefault` d

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

entryEdited	:: CmdArrow AlbumTree AlbumTree
entryEdited 	= ( getNode >>> isA picEdited )
		  `guards`
		  this

isEditedAlbum	:: CmdArrow AlbumTree AlbumTree
isEditedAlbum	= ( entryEdited <+> (getChildren >>> entryEdited) )
		  `guards`
		  this

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
    = runAction ("check album loaded for " ++ showPath p) $
      changeAlbums (processTree (const this)) $ p

loadAndCheckAlbum	:: PathArrow a AlbumTree
loadAndCheckAlbum p
    = runAction ("check for entry loaded " ++ showPath p) $
      ( changeAlbums (processTree (const this))
	/>>>/
	checkPath
      )
      $ p

loadAllAlbums	:: PathArrow a AlbumTree
loadAllAlbums p
    = runAction ("check for all entries loaded " ++ showPath p) $
      changeAlbums (processTreeSelfAndDesc (const this)) $ p

-- ------------------------------------------------------------

{- not yet ready: single file for every picture

storeAllChangedEntries	:: PathArrow AlbumTree AlbumTree
storeAllChangedEntries
    = changeAlbums $
      processTreeDescAndSelfUC storeChangedEntries

storeChangedEntries	:: PathArrow AlbumTree AlbumTree
storeChangedEntries p
    = ( ( ( runAction ("storing all entries at: " ++ showPath p) $
	    ( storeEntryOK			-- store the changes
	      >>>
	      clearEdited			-- clear edited marks
	      >>>
	      updateNode (arr clearPic)		-- and remove all image info 
	    )
	    `orElse` this			-- errors when writing the album
	  )
	  `when` entryEdited			-- some album entries have been changed
	)
	>>>
	unloadSubEntries p
      )
      `when` isEntryLoaded			-- only albums already loaded are of interest
    where
    storeEntryOK
	= storeEntry $<<< ( (getNode >>^ isAl) &&& getConfig (albumPath p) &&& get theConfigName )

unloadSubEntries		:: PathArrow AlbumTree AlbumTree
unloadSubEntries p
    = ( runAction ("unloading entry: " ++ showPath p) $
	processChildren ( setChildren []
			  >>>
			  updateNode (arr clearPic)
			)
      )
      `whenNot`
      deep entryEdited

storeEntry	:: Bool -> FilePath -> FilePath -> CmdArrow AlbumTree AlbumTree
storeEntry isAlb doc conf
    = perform mkdir
      >>>
      ( storeDocData xpAlbumTree rootName doc conf
	`guards`
	updateNode ( arr $ change theRef (const doc) )
      )
    where
    mkdir = constA doc >>> arrIOE mkDirectoryPath
    rootName
	| isAlb		= "album"
	| otherwise	= "picture"
-}
-- ------------------------------------------------------------
-- store the album and all subalbums adressed by a path and mark them as unloaded

storeAllChangedAlbums	:: PathArrow AlbumTree AlbumTree
storeAllChangedAlbums
    = changeAlbums $
      processTreeDescAndSelfUC storeChangedAlbums

storeChangedAlbums	:: PathArrow AlbumTree AlbumTree
storeChangedAlbums p
    = ( ( ( runAction ("storing all albums at: " ++ showPath p) $
	    ( storeAlbumOK			-- store the changes
	      >>>
	      clearEditedChildren		-- and clear edited marks
	    )
	    `orElse` this			-- errors when writing the album
	  )
	  `when` isEditedAlbum			-- some album entries have been changed
	)
	>>>
	unloadSubAlbums p
      )
      `when` (isAlbum >>> isEntryLoaded)	-- only albums already loaded are of interest
    where
    clearEditedChildren
	= processChildren clearEdited
    storeAlbumOK
	= storeAlbum $<< ( getConfig (albumPath p) &&& get theConfigName )

unloadSubAlbums		:: PathArrow AlbumTree AlbumTree
unloadSubAlbums p
    = ( runAction ("unloading album: " ++ showPath p) $
	processChildren (setChildren [])
      )
      `whenNot`
      (getChildren >>> deep entryEdited)

storeAlbum	:: FilePath -> FilePath -> CmdArrow AlbumTree AlbumTree
storeAlbum doc conf
    = storeDocData xpAlbumTree "album" doc conf
      `guards`
      updateNode ( arr $ change theRef (const doc) )

storeDocData	:: PU b -> String -> FilePath -> FilePath -> CmdArrow b XmlTree
storeDocData p rootName doc conf
    = runAction ("pickle document: " ++ doc)
                ( xpickleVal p ) 
      >>>
      runAction ("write document:  " ++ doc)
		( addDoctypeDecl rootName "" (pathFromTo doc (dirPath conf </> "archive.dtd"))
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

storeConfig	:: CmdArrow b XmlTree
storeConfig
    = storeC $< get theConfigName
    where
    storeC doc
	= runAction ("storing configuration: " ++ doc)
	            ( get theConfig
		      >>>
		      storeDocData xpConfig "config" doc doc
		    )

storeArchive	:: CmdArrow b XmlTree
storeArchive
    = storeA $<< (get theArchiveName &&& get theConfigName)
    where
    storeA doc confName
	= runAction ("storing the archive: " ++ doc)
	  ( get theAlbums
	    >>>
	    withRootDir cutA
	    >>>
	    arr (Archive confName)
	    >>>
	    storeDocData xpArchive "archive" doc confName
	  )
    cutA p
	= setChildren []
	  >>>
	  ( (\ doc -> updateNode (arr $ change theRef (const doc))) $< getConfig (albumPath p) )

-- ------------------------------------------------------------
--
-- check whether an album is loaded, if not yet done, load the album

checkEntryLoaded	:: CmdArrow AlbumTree AlbumTree
checkEntryLoaded
    = ( getExtAlbumRef
	>>>
	( loadAlbum $<< get theArchiveName &&& this )
      )
      `orElse` this

isEntryLoaded		:: CmdArrow AlbumTree AlbumTree
isEntryLoaded		= neg getExtAlbumRef `guards` this

-- check whether an entry addresed by a path exists
-- in a tree

checkPath	:: PathArrow AlbumTree AlbumTree
checkPath p
    = runAction ("checking path: " ++ showPath p) $
      getTree p `guards` this

-- ------------------------------------------------------------
--
-- traversal functions for reading the tree

getTree	:: PathArrow AlbumTree AlbumTree
getTree	= getTreeAndProcess (const this)

getTreeAndProcess	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcess pa p0
    = getSub p0
    where
    getSub p
	| null p	= none
	| null p'	= nodeMatch `guards` (checkAndProcess pa p0)
	| otherwise     = nodeMatch `guards` (checkEntryLoaded >>> getChildren >>> getSub p')
	where
	(n' : p') = p
	nodeMatch = hasPicId n'

getTreeAndProcessChildren	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessChildren 	= getTreeAndProcess . (getChildrenAndProcess getChildren)

getTreeAndProcessChildrenC	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessChildrenC 	= getTreeAndProcess . (getChildrenAndProcess getChildrenAndCheck)

getTreeAndProcessDesc		:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessDesc	 	= getTreeAndProcess . (getDescAndProcess getChildren)

getTreeAndProcessDescC		:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessDescC	 	= getTreeAndProcess . (getDescAndProcess getChildrenAndCheck)

getTreeAndProcessSelfAndDesc	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessSelfAndDesc	 = getTreeAndProcess . (getSelfAndDescAndProcess getChildren)

getTreeAndProcessSelfAndDescC	:: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessSelfAndDescC	 = getTreeAndProcess . (getSelfAndDescAndProcess getChildrenAndCheck)

getChildrenAndProcess		:: CmdArrow AlbumTree AlbumTree ->
				   PathArrow AlbumTree b -> PathArrow AlbumTree b
getChildrenAndProcess children pa p
    = children		-- optionally check album loaded
      >>>
      ( pa $< (getPicId >>^ (reverse . (:rp))) )
    where
    rp = reverse p	-- make p ++ [n] a bit more efficient

getDescAndProcess		:: CmdArrow AlbumTree AlbumTree ->
				   PathArrow AlbumTree b -> PathArrow AlbumTree b
getDescAndProcess children pa p
    = getD (reverse p)	-- make p ++ [n] a bit more efficient
    where
    getD rp = children
	      >>>
	      ( (\ rp' -> pa (reverse rp') <+> getD rp') $< (getPicId >>^ (:rp)) )

getSelfAndDescAndProcess	:: CmdArrow AlbumTree AlbumTree ->
				   PathArrow AlbumTree b -> PathArrow AlbumTree b
getSelfAndDescAndProcess children pa p
    = pa p
      <+>
      getDescAndProcess children pa p

getChildrenAndCheck		:: CmdArrow AlbumTree AlbumTree
getChildrenAndCheck		= getChildren >>> checkEntryLoaded

-- ----------------------------------------
--
-- | process all nodes of a tree

processAll	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processAll pa p
    = pa p
      >>>
      processChildren ( (\ n -> processAll pa (p ++ [n])) $< getPicId )

-- ----------------------------------------
--
-- | process a tree addressed by a path
--   with an arrow getting the full path as parameter

processTree	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTree pa p0
    = processSub p0
    where
    processSub p
	| null p	= this
	| null p'	= checkAndProcess pa p0
                          `when`
			  nodeMatch
	| otherwise	= ( checkEntryLoaded >>> processChildren (processSub p') )
			  `when`
			  nodeMatch
	where
	(n' : p') = p
	nodeMatch = hasPicId n'

processTreeChildren		:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeChildren		= processTree . selChildrenAndProcess . checkAndProcess

processTreeDescTD		:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeDescTD		= processTree . selDescAndProcessTD . checkAndProcess

processTreeDescBU		:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeDescBU		= processTree . selDescAndProcessBU . checkAndProcess

processTreeSelfAndDesc		:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeSelfAndDesc		= processTree . selSelfAndDescAndProcessTD . checkAndProcess

processTreeDescAndSelfUC	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeDescAndSelfUC	= processTree . selSelfAndDescAndProcessBU	-- don't load unloaded subalbums

processTreeDescAndSelf		:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeDescAndSelf		= processTree . selSelfAndDescAndProcessBU . checkAndProcess

checkAndProcess			:: PathArrow AlbumTree b         -> PathArrow AlbumTree b
checkAndProcess pa p		= checkEntryLoaded >>> pa p

selChildrenAndProcess		:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selChildrenAndProcess pa p
    = processChildren
      ( pa $< (getPicId >>^ (reverse . (:rp))) )
    where
    rp = reverse p	-- make p ++ [n] a bit more efficient

selDescAndProcessTD		:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selDescAndProcessTD pa p
    = processD (reverse p)	-- make p ++ [n] a bit more efficient
    where
    processD rp = processChildren
		  ( (\ rp' -> pa (reverse rp') >>> processD rp') $< (getPicId >>^ (:rp)) )

selDescAndProcessBU		:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selDescAndProcessBU pa p
    = processD (reverse p)	-- make p ++ [n] a bit more efficient
    where
    processD rp = processChildren
		  ( (\ rp' -> processD rp' >>> pa (reverse rp')) $< (getPicId >>^ (:rp)) )

selSelfAndDescAndProcessTD	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selSelfAndDescAndProcessTD pa p
    = pa p
      >>>
      selDescAndProcessTD pa p

selSelfAndDescAndProcessBU	:: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selSelfAndDescAndProcessBU pa p
    = selDescAndProcessBU pa p
      >>>
      pa p

-- ----------------------------------------

hasPicId	:: Name -> CmdArrow AlbumTree AlbumTree
hasPicId n    = (getPicId >>> isA (==n)) `guards` this

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
withConfig c p	= ( \ conf -> c conf p ) $< get theConfig

changeAlbums	:: PathArrow AlbumTree AlbumTree -> PathArrow a AlbumTree
changeAlbums pa p
    		= get theAlbums >>> pa p >>> set theAlbums

isAlbum		:: CmdArrow AlbumTree AlbumTree
isAlbum		= ( getNode >>> isA isAl ) `guards` this

getExtAlbumRef	:: CmdArrow AlbumTree String
getExtAlbumRef	= getNode >>> picRef ^>> isA (not . null)

-- ----------------------------------------

getRelatives	:: PathArrow AlbumTree (Path, Path, Path)
getRelatives p
    = runAction ("get relatives: " ++ showPath p) $
      getPN ( if null p then emptyPath else init p )
    where
    n = last p
    getPN pp
	= ( listA ( getTree pp
		    >>>
		    getChildren
		    >>>
		    getPicId
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

getAllWithAttr		:: String -> String -> PathArrow AlbumTree (Path, String, String)
getAllWithAttr rek rev
    = getTreeAndProcessSelfAndDescC $
      ( const ( getNode
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
	      )
	/>>>/
	(\ p -> arr (\ (k, v) -> (p, k, v)))
      )

-- ------------------------------------------------------------

addAlbumEntry	:: AlbumEntry -> CmdArrow AlbumTree AlbumTree
addAlbumEntry (p0, pic)
    = insert p0
    where
    n = picId pic
    insert	:: PathArrow AlbumTree AlbumTree
    insert p
	| null p	= setNode pic `when` hasPicId n			-- entry already in tree
	| null p'	= replaceChildren (getChildren <+> mkPic pic)	-- append a new leave to the children
			  `when`
			  hasPicId n'
	| otherwise	= processChildren (insert p')			-- descend into tree
			  `when`
			  hasPicId n'
	where
	(n'  : p' ) = p

removeAlbumEntry	:: PathArrow AlbumTree AlbumTree
removeAlbumEntry p
    = processTree (const none) p

-- ------------------------------------------------------------
-- update an attribute for an album or picture

updateAttrKeys	:: PathArrow AlbumTree AlbumTree
updateAttrKeys p
    = runAction ("updating attribute keys for " ++ showPath p) $
      editNode (arr $ change theAttrs normAttrs)

updateAttr	:: String -> String -> PathArrow AlbumTree AlbumTree
updateAttr an av p
    = runAction ("updating " ++ showPath p ++ " attr " ++ show an ++ " with value " ++ show av) $
      editNode (arr $ change theAttrs (mergeAttr an av))

updateAttrs	:: Attrs -> PathArrow AlbumTree AlbumTree
updateAttrs am p
    = runAction ("updating " ++ showPath p ++ " attributes " ++ show am) $
      editNode (arr $ change theAttrs (mergeAttrs am))

updateExifAttrs	:: ConfigArrow AlbumTree AlbumTree
updateExifAttrs c p
    = runAction ("updating " ++ showPath p ++ " exif attributes") $
      editNode (arrIOE (importExifAttrs c p))

-- ------------------------------------------------------------
--
-- rename picture

renamePic	:: Name -> ConfigArrow AlbumTree AlbumTree
renamePic nn c p
    = runAction ("renaming " ++ showPath p ++ " to " ++ nn)
      ( checkEntryLoaded
	>>>
	editNode (arrIOE (mvPic nn c p))
      )

renameContent	:: ConfigArrow AlbumTree AlbumTree
renameContent c p
    = runAction ("renaming album contents for " ++ showPath p)
      ( checkEntryLoaded
	>>>
	( rename $< listA (getChildren >>> getPicId) )
      )
    where
    rename	:: [Name] -> CmdArrow AlbumTree AlbumTree
    rename ids
	| null nameMap
	    = this
	| otherwise		-- rename in 2 steps, to prevent name clashes
	    = processChildren (renameChild nameMap1 $< getPicId)
	      >>>
	      processChildren (renameChild nameMap2 $< getPicId)
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

        nameMap1	= map (\ (o,  n) -> (o, "#" ++ n)) $ nameMap
        nameMap2	= map (\ (_o, n) -> ("#" ++ n, n)) $ nameMap

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
      ( checkEntryLoaded
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

-- ------------------------------------------------------------
