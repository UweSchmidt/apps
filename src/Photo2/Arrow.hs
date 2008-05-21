module Photo2.Arrow
where

import Data.Maybe
-- import Data.Tree.Class(mkLeaf)

import Text.XML.HXT.Arrow

import Photo2.ArchiveTypes


-- ------------------------------------------------------------

type CmdArrow a b = IOStateArrow AppState a b

runCmd	:: CmdArrow a b -> AppState -> IO AppState
runCmd cmd s0
    = do
      (s1, _res) <- runIOSLA cmd (initialState s0) undefined
      return (xio_userState s1)

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
getField gf	= getUserState >>> arr gf

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

selAlbums	= (albums, \ x s -> s {albums = x})
selArchiveName	= (archiveName, \ x s -> s {archiveName = x})
selConfig	= (config, \ x s -> s {config = x})
selOptions	= (options, \ x s -> s {options = x})
selStatus	= (status, \ x s -> s {status = x})
selChanged	= (changed, \ x s -> s {changed = x})
selOption k	= (lookup1 k, \ v os -> addEntry k v os) `sub` selOptions

theOptions	= mkSelA $ selOptions
theOption k	= mkSelA $ selOption k
theAlbums	= mkSelA $ selAlbums
theConfig	= mkSelA $ selConfig
theStatus	= mkSelA $ selStatus
theChangedFlag	= mkSelA $ selChanged
theArchiveName	= mkSelA $ selArchiveName

-- ------------------------------------------------------------

setComp	:: SelArrow a b -> b -> CmdArrow a a
setComp c v	= perform $ constA v >>> set c

done		:: CmdArrow a a
done		= setComp theStatus Nothing

failed		:: String -> CmdArrow a a
failed msg	= setComp theStatus (Just msg)

setChanged	:: CmdArrow a a
setChanged	= setComp theChangedFlag True

clearChanged	:: CmdArrow a a
clearChanged	= setComp theChangedFlag False

setArchiveName	:: String -> CmdArrow a a
setArchiveName n = setComp theArchiveName n

-- ------------------------------------------------------------

withStatusCheck	:: String -> CmdArrow a b -> CmdArrow a b
withStatusCheck msg action
    = traceMsg 0 ("starting: " ++ msg)
      >>>
      failed msg
      >>>
      ( ( action
	  >>>
	  done
	  >>>
	  traceMsg 0 ("finished: " ++ msg)
	)
	`orElse`
	( traceMsg 0 ("failed: " ++ msg)
	  >>>
	  none
	)
      )

whenStatusOK	:: String -> CmdArrow a b -> CmdArrow a b
whenStatusOK msg action
    = ( ( get theStatus >>> isA (isNothing) )
	`guards`
	action
      )
      `orElse`
      ( traceMsg 0 ("status error, not performed: " ++ msg)
	>>>
	none
      )

withStatusOK	:: String -> CmdArrow a b -> CmdArrow a b
withStatusOK msg action
    = whenStatusOK msg (withStatusCheck msg action)

-- ------------------------------------------------------------

loadDocData	:: PU b -> String -> CmdArrow a b
loadDocData p doc
    = xunpickleDocument p [ (a_remove_whitespace, v_1)
			  , (a_validate, v_0)
			  , (a_tagsoup, v_1)
			  ] doc
      >>>
      perform ( xpickleDocument p [ (a_indent, v_1)
				  ] ""
	      )
      >>>
      traceMsg 0 ("document loaded and unpickled: " ++ show doc)

loadArchive	:: String -> CmdArrow a Archive
loadArchive doc
    = withStatusOK ("loading archive: " ++ show doc)
      $
      loadDocData xpArchive doc
      >>>
      setArchiveName doc
      >>>
      setChanged

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

processTreeByPath	:: Path -> CmdArrow AlbumTree AlbumTree ->  CmdArrow AlbumTree AlbumTree
processTreeByPath p pa
    | null p	= this
    | null p'	= pa `when` hasPicName n'
    | otherwise	= processChildren (processTreeByPath p' pa)
    where
    (n' : p') = p

hasPicName	:: Name -> CmdArrow AlbumTree AlbumTree
hasPicName n
    = (getNode >>> arr picId >>> isA (==n)) `guards` this

mkPic	:: Pic -> CmdArrow AlbumTree AlbumTree
mkPic	= mkLeaf

getAlbumEntry	:: Path -> CmdArrow AlbumTree AlbumEntry
getAlbumEntry p
    | null p	= none
    | otherwise	= getDescByPath p'
		  >>>
		  hasPicName n'
		  >>>
		  getNode
		  >>>
		  arr (\ x -> (p, x))
    where
    n' = last p
    p' = init p

getAlbumPaths	:: Path -> CmdArrow AlbumTree Path
getAlbumPaths p
    = getDescByPath p
      >>>
      getPaths (reverse p)
      >>>
      arr reverse
    where
    getPaths :: Path -> CmdArrow AlbumTree Path
    getPaths p'
	= getPaths' $< (getNode >>> arr picId)
	  where
	  getPaths' :: Name -> CmdArrow AlbumTree Path
	  getPaths' n'
	      = constA p''
		<+>
		( getChildren >>> getPaths p'' )
		where
		p'' = n' : p'

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
    = processTreeByPath p none

-- ------------------------------------------------------------

