module Photo2.Arrow
where

import Text.XML.HXT.Arrow

import Photo2.State (Fields(..), initialFields)
import Photo2.ArchiveTypes
import Photo2.DataModell

type CmdArrow a b = IOStateArrow Fields a b

type CmdState	= Fields

runCmd	:: CmdArrow a b -> CmdState -> IO CmdState
runCmd cmd s0
    = do
      (s1, _res) <- runIOSLA cmd (initialState s0) undefined
      return (xio_userState s1)

getAlbum	:: CmdArrow b AlbumDir
getAlbum	= getUserState >>> arr albumDir'

getConfig	:: CmdArrow b Config
getConfig	= getUserState >>> arr config'

setConfig	:: CmdArrow Config Config
setConfig
    = perform ( ( this &&& getUserState )
		>>> arr (\ ~(c, s) -> s {config' = c})
		>>> setUserState
	      )

type Getter s a	= s -> a
type Setter s a	= a -> s -> s
type Selector s a = (Getter s a, Setter s a)

setField	:: Setter CmdState b -> CmdArrow b b
setField sf	= perform ( ( this &&& getUserState )
			    >>> arr (uncurry sf)
			    >>> setUserState
			  )

getField	:: Getter CmdState b -> CmdArrow a b
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

mkSelA	:: Selector CmdState b -> SelArrow a b
mkSelA (g, s) = SA { get = getField g
		       , set = setField s
		       }

selAlbums	= (albumDir', \ x s -> s {albumDir' = x})
selOptions	= (options', \ x s -> s {options' = x})
selOption k	= (lookup1 k, \ v os -> addEntry k v os) `sub` selOptions

theOptions	= mkSelA $ selOptions
theOption k	= mkSelA $ selOption k
theAlbums	= mkSelA $ selAlbums
