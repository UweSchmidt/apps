module Photo2.Arrow
where

import Data.Maybe ()

import Text.XML.HXT.Arrow

import Photo2.ArchiveTypes
import Photo2.FilePath
import Photo2.Config

-- ------------------------------------------------------------

type CmdArrow    a b =           IOStateArrow AppState a b
type PathArrow   a b = Path   -> CmdArrow  a b
type ConfigArrow a b = Config -> PathArrow a b

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

theOptions      :: SelArrow a Options
theOptions	= mkSelA $ selOptions

theOption       :: Name -> SelArrow a String
theOption k	= mkSelA $ selOption k

theAlbums       :: SelArrow a AlbumTree
theAlbums	= mkSelA $ selAlbums

theConfig       :: SelArrow a Config
theConfig	= mkSelA $ selConfig

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
      traceS $<< (get theStatus &&& get (theOption "debug"))
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
    = runAction ("check loaded albums for " ++ show (joinPath p)) $
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
    = runAction ("storing all albums at: " ++ show (joinPath p)) $
      ( processChildren (storeSubAlbums $< getPicId)
	>>>
	( ( ( removeSubAlbums
	      >>>
	      ( storeAlbum $<< ( getConfig (albumPath p) &&& get theConfigName ) )
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
	= storeAlbumTree (p ++ [n])
	  `when`
	  getChildren
    removeSubAlbums 
	= processChildren (setChildren [])

storeAlbum	:: FilePath -> FilePath -> CmdArrow AlbumTree AlbumTree
storeAlbum doc config
    = storeDocData xpAlbumTree "album" doc config
      `guards`
      changeNode (\ n -> n {picRef = doc})

storeDocData	:: PU b -> String -> FilePath -> FilePath -> CmdArrow b XmlTree
storeDocData p root doc config
    = runAction ("pickle document: " ++ doc)
                ( xpickleVal p ) 
      >>>
      runAction ("write document:  " ++ doc)
		( addDoctypeDecl root "" (pathFromTo doc (dirName config </> "archive.dtd"))
		  >>>
		  writeDocument [ (a_indent, v_1)
				, (a_output_encoding, isoLatin1)
				] ( doc ++ ".new" )
		)
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
	  ( (\ doc -> (changeNode (\ n -> n {picRef = doc}))) $< getConfig (albumPath p))

-- ------------------------------------------------------------
--
-- check whether an album is loaded, if not yet done, load the album

checkAlbum	:: PathArrow AlbumTree AlbumTree
checkAlbum _p
    = ( getNode
	>>>
	picRef
	^>>
	isA (not . null)
	>>>
	( loadAlbum $<< get theArchiveName &&& this )
      )
      `orElse` this

-- check whether an entry addresed by a path exists

checkPath	:: PathArrow AlbumTree AlbumTree
checkPath p
    = runAction ("checking path: " ++ show (joinPath p)) $
      getTreeByPath p

-- ------------------------------------------------------------

getTreeByPath	:: PathArrow AlbumTree AlbumTree
getTreeByPath p
    | null p	= none
    | null p'	= nodeMatch `guards` this
    | otherwise = nodeMatch `guards` (getChildren >>> getTreeByPath p')
    where
    (n' : p') = p
    nodeMatch = hasPicName n'

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
withDir p c	= (\ p' -> withAbsDir p' c) $< (get theWd >>> arr (++ p))

withAbsDir	:: Path -> PathArrow a b -> CmdArrow a b
withAbsDir p c	= c p

withCwd		:: PathArrow a b -> CmdArrow a b
withCwd		= withDir []

withRootDir	:: PathArrow a b -> CmdArrow a b
withRootDir c	= (\ p' -> withAbsDir p' c) $< getRootPath

mkAbs		:: CmdArrow Path Path
mkAbs		= (\ wd -> arr (wd ++)) $< get theWd

withConfig	:: ConfigArrow a b -> PathArrow a b
withConfig c p	= ( \ config -> c config p ) $< get theConfig

-- ----------------------------------------

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

getAllAlbumPaths	:: PathArrow AlbumTree Path
getAllAlbumPaths
    = getPaths gp
    where
    gp :: PathArrow AlbumTree Path
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

getAlbumPaths	:: PathArrow AlbumTree Path
getAlbumPaths
    = getPaths (\ p' -> getPicId >>> arr (:p') )

getPaths	:: PathArrow AlbumTree Path -> PathArrow AlbumTree Path
getPaths gp p
    = getDescByPath p
      >>>
      gp (reverse p)
      >>^
      reverse


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
--
-- update image data for a single node, the arrow input

updatePic	:: ConfigArrow AlbumTree AlbumTree
updatePic config p
    = runAction ("updating " ++ show (joinPath p)) $
      this

-- update all entries addresed by a path

updateAllPics	:: PathArrow AlbumTree AlbumTree
updateAllPics	= processTreeByPath (processAllByPath (withConfig updatePic))

-- ------------------------------------------------------------
