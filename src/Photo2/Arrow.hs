module Photo2.Arrow
where

import Data.Maybe
-- import Data.Tree.Class(mkLeaf)

import Text.XML.HXT.Arrow

import Photo2.ArchiveTypes
import Photo2.FilePath
import Photo2.Config

-- ------------------------------------------------------------

type CmdArrow a b = IOStateArrow AppState a b

runCmd	:: CmdArrow a b -> AppState -> IO AppState
runCmd cmd s0
    = do
      (s1, _res) <- runIOSLA (setTraceLevel 0 >>> cmd) (initialState s0) undefined
      return (xio_userState s1)

runCmd'	:: CmdArrow a b -> ([b] -> IO ()) -> AppState -> IO AppState
runCmd' cmd out
    = runCmd (listA cmd >>> arrIO out)

-- ------------------------------------------------------------

type Getter s a	= s -> a
type Setter s a	= a -> s -> s
type Selector s a = (Getter s a, Setter s a)

setField	:: Setter AppState b -> CmdArrow b b
setField sf	= perform ( ( this &&& getUserState )
			    >>> arr (uncurry sf)
			    >>> setUserState
			  )

getField	:: Getter AppState b -> CmdArrow a b
getField gf	= getUserState >>^ gf

sub	:: Selector b c -> Selector a b -> Selector a c
sub (g2, s2) (g1, s1)
    = ( g2 . g1
      , s1s2
      )
    where
    s1s2 x s
	= s'
	where
	x1  = g1 s
	x1' = s2 x x1
	s'  = s1 x1' s

data SelArrow a b = SA { get :: CmdArrow a b
		       , set :: CmdArrow b b
		       }

mkSelA	:: Selector AppState b -> SelArrow a b
mkSelA (g, s) = SA { get = getField g
		   , set = setField s
		   }

selAlbums	= (albums,      \ x s -> s {albums = x})
selArchiveName	= (archiveName, \ x s -> s {archiveName = x})
selConfig	= (config,      \ x s -> s {config = x})
selConfigName	= (configName,  \ x s -> s {configName = x})
selOptions	= (options,     \ x s -> s {options = x})
selStatus	= (status,      \ x s -> s {status = x})
selWd		= (cwd,         \ x s -> s {cwd = x})
selOption k	= (lookup1 k,   \ v os -> addEntry k v os) `sub` selOptions

theOptions	= mkSelA $ selOptions
theOption k	= mkSelA $ selOption k
theAlbums	= mkSelA $ selAlbums
theConfig	= mkSelA $ selConfig
theStatus	= mkSelA $ selStatus
theArchiveName	= mkSelA $ selArchiveName
theConfigName	= mkSelA $ selConfigName
theWd           = mkSelA $ selWd

-- ------------------------------------------------------------

getConfig	:: (Config -> a) -> CmdArrow b a
getConfig cf	= get theConfig >>^ arr cf

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
      traceS $< get theStatus
    where
    traceS st	= traceMsg 0 (replicate (level st) ' ' ++ msg)
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
    = ( statusOK `guards` ( runAction $< get theStatus ) )
      `orElse`
      ( traceStatus ("error   : " ++ msg)
	>>>
	none
      )
    where
    runAction oldStatus
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

loadAlbums	:: Path -> CmdArrow a AlbumTree
loadAlbums p
    = get theAlbums
      >>>
      processAllNodesOnPath checkAlbum p
      >>>
      set theAlbums

loadAllAlbums	:: Path -> CmdArrow a AlbumTree
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

storeAllAlbums	:: Path -> CmdArrow a AlbumTree
storeAllAlbums p
    = get theAlbums
      >>>
      processTreeByPath storeAlbumTree p
      >>>
      set theAlbums

storeAlbumTree	:: Path -> CmdArrow AlbumTree AlbumTree
storeAlbumTree p
    = runAction ("storing all albums at: " ++ show (joinPath p)) $
      ( processChildren (storeSubAlbums $< getPicId)
	>>>
	( ( ( removeSubAlbums
	      >>>
	      ( storeAlbum $< getConfig (albumPath p) )
	      >>>
	      changeNode (\ n -> n {picEdited = False})
	    )
	    `orElse`
	    this	-- something went wrong
	  )
	  `when`
	  entryChanged
	)
      )
      `when`
      isAlbum
    where
    storeSubAlbums n
	= storeAlbumTree (p ++ [n]) `when` isAlbum
    removeSubAlbums 
	= processChildren (setChildren [])

storeAlbum	:: FilePath -> CmdArrow AlbumTree AlbumTree
storeAlbum doc
    = storeDocData xpAlbumTree doc
      `guards`
      changeNode (\ n -> n {picRef = doc})

storeDocData	:: PU b -> String -> CmdArrow b XmlTree
storeDocData p doc
    = runAction ("pickle document: " ++ doc) (xpickleVal p)
      >>>
      runAction ("write document:  " ++ doc)
		    (writeDocument [ (a_indent, v_1)
				   , (a_output_encoding, isoLatin1)
				   ] "tmp.xml" {-doc-})
      >>>
      documentStatusOk
      >>>
      traceStatus ("stored  : " ++ show doc)

isAlbum		:: CmdArrow AlbumTree AlbumTree
isAlbum		=  ( getChildren `guards` this )
		   <+>
		   ( (getNode >>> arr picRef >>> isA (not . null)) `guards` this )

entryChanged	:: CmdArrow AlbumTree AlbumTree
entryChanged 	= this -- (getNode >>> arr picEdited >>> isA (==True))
		  `guards`
		  this

-- ------------------------------------------------------------
--
-- check whether an album is loaded, if not yet done, load the album

checkAlbum	:: Path -> CmdArrow AlbumTree AlbumTree
checkAlbum p
    = ( getNode
	>>>
	picRef
	^>>
	isA (not . null)
	>>>
	( loadAlbum $<< get theArchiveName &&& this )
      )
      `orElse` this
    where
    ps = joinPath p

-- check whether an entry addresed by a path exists

checkPath	:: Path -> CmdArrow AlbumTree AlbumTree
checkPath p
    = runAction ("checking path: " ++ show (joinPath p)) $
      getTreeByPath p

-- ------------------------------------------------------------

getTreeByPath	:: Path -> CmdArrow AlbumTree AlbumTree
getTreeByPath p
    | null p	= none
    | null p'	= nodeMatch `guards` this
    | otherwise = nodeMatch `guards` (getChildren >>> getTreeByPath p')
    where
    (n' : p') = p
    nodeMatch = hasPicName n'

getDescByPath	:: Path -> CmdArrow AlbumTree AlbumTree
getDescByPath p
    | null p	= this
    | otherwise = nodeMatch `guards` (getChildren >>> getDescByPath p')
    where
    (n' : p') = p
    nodeMatch = hasPicName n'
{-
processTreeByPath	:: (Path -> CmdArrow AlbumTree AlbumTree) -> Path -> CmdArrow AlbumTree AlbumTree
processTreeByPath pa p
    | null p	= this
    | null p'	= pa p `when` hasPicName n'
    | otherwise	= processChildren (processTreeByPath pa p')
    where
    (n' : p') = p
-}
processAllNodesOnPath	:: (Path -> CmdArrow AlbumTree AlbumTree) -> Path -> CmdArrow AlbumTree AlbumTree
processAllNodesOnPath pa p
    | null p	= this
    | null p'	= pa p `when` hasPicName n'
    | otherwise	= ( pa p >>> processChildren (processAllNodesOnPath pa p') ) `when` hasPicName n'
    where
    (n' : p') = p

-- ----------------------------------------
--
-- | process all nodes of a tree

processAllByPath	:: (Path -> CmdArrow AlbumTree AlbumTree) -> Path -> CmdArrow AlbumTree AlbumTree
processAllByPath pa p
    = pa p
      >>>
      ( (\ n -> processChildren $ processAllByPath pa (p ++ [n])) $< getPicId )

-- ----------------------------------------
--
-- | process all nodes of a tree addressed by a path
--   with an arrow getting the full path as parameter

processAllSubTreesByPath	:: (Path -> CmdArrow AlbumTree AlbumTree) -> Path -> CmdArrow AlbumTree AlbumTree
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

processTreeByPath	:: (Path -> CmdArrow AlbumTree AlbumTree) -> Path -> CmdArrow AlbumTree AlbumTree
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

withCwd		:: (Path -> CmdArrow a b) -> CmdArrow a b
withCwd		= withDir []

withDir		:: Path -> (Path -> CmdArrow a b) -> CmdArrow a b
withDir p c	= c $< (getRootPath >>> arr (++ p))

withAbsDir	:: Path -> (Path -> CmdArrow a b) -> CmdArrow a b
withAbsDir p c	= c p

mkAbs		:: CmdArrow Path Path
mkAbs		= (\ wd -> arr (wd ++)) $< get theWd
 
-- ----------------------------------------

getAlbumEntry	:: Path -> CmdArrow AlbumTree AlbumEntry
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

getAllAlbumPaths	:: Path -> CmdArrow AlbumTree Path
getAllAlbumPaths
    = getPaths gp
    where
    gp :: Path -> CmdArrow AlbumTree Path
    gp p'
	= gp' $< getPicId
	  where
	  gp' :: Name -> CmdArrow AlbumTree Path
	  gp' n'
	      = constA p''
		<+>
		( getChildren >>> gp p'' )
		where
		p'' = n' : p'

getAlbumPaths	:: Path -> CmdArrow AlbumTree Path
getAlbumPaths
    = getPaths (\ p' -> getPicId >>> arr (:p') )

getPaths	:: (Path -> CmdArrow AlbumTree Path) -> Path -> CmdArrow AlbumTree Path
getPaths gp p
    = getDescByPath p
      >>>
      gp (reverse p)
      >>^
      reverse


addAlbumEntry	:: AlbumEntry -> CmdArrow AlbumTree AlbumTree
addAlbumEntry (p, pic)
    = insert p
    where
    n = picId pic
    insert	:: Path -> CmdArrow AlbumTree AlbumTree
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

removeAlbumEntry	:: Path -> CmdArrow AlbumTree AlbumTree
removeAlbumEntry p
    = processTreeByPath (const none) p

-- ------------------------------------------------------------
