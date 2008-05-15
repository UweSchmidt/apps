module Photo2.State
where

import Control.Monad.State
    ( StateT
    , gets
    , modify
    , liftIO
    , runStateT
    )

import Data.AssocList

import Photo2.ArchiveTypes
import Photo2.DataModell

-- ------------------------------------------------------------
-- the global state

data Fields = Fields { albumDir' :: AlbumDir
		     , config'   :: Config
		     , options'  :: Options
		     }

initialFields	:: Fields
initialFields	= Fields { albumDir' = emptyAlbumDir
			 , config'   = emptyConfig
			 , options'  = []
			 }

type AState a = StateT Fields IO a

type Selector a = (AState a, a -> AState ())

sub	:: (a -> b, b -> a -> a) -> Selector a -> Selector b
sub (a, m) (g, s)
    = (g', s')
    where
    g' = do
	 v <- g
	 return (a v)
    s' y = do
	   v <- g
	   s $ m y v

get :: Selector a -> AState a
get = fst

set :: Selector a -> a -> AState ()
set = snd

update :: Selector a -> (a -> a) -> AState ()
update (g, s) f = do v <- g
		     s (f v)

io :: IO a -> AState a
io = liftIO

-- ------------------------------------------------------------

albumDir :: Selector AlbumDir
albumDir = (gets albumDir', \x -> modify (\vs -> vs {albumDir' = x}))
 
config :: Selector Config
config = (gets config', \x -> modify (\vs -> vs {config' = x}))
 
options :: Selector Options
options = (gets options', \x -> modify (\vs -> vs {options' = x}))

theOption	:: Name -> Selector Value
theOption k	= (lookup1 k, \ v os -> addEntry k v os) `sub` options

theAlbum	:: Path -> Selector (Maybe Pic)
theAlbum p	= (getPic p, \ v ad -> setPic p v ad) `sub` albumDir

-- setPic p v ad	= undefined

{-
sv3  :: Selector Int
sv3  = (sv', \ x y -> y {sv' = x}) `sub` options
-}

-- ------------------------------------------------------------

runApp	:: AState () -> IO ()
runApp app
	   = do
	     runStateT app initialFields
	     return ()

-- ------------------------------------------------------------
