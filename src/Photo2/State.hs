module Photo2.State
where

import Control.Monad.State
    ( StateT
    , runStateT
    )
import qualified Control.Monad.State as CMS

import Data.AssocList
import Data.Maybe

import Text.XML.HXT.Arrow

import Photo2.ArchiveTypes
import Photo2.DataModell

-- ------------------------------------------------------------
--
-- the global state

data Fields = Fields { albums   :: AlbumTree
		     , config   :: Config
		     , options  :: Options
		     , status   :: Status
		     }

type Status = Maybe String

initialFields	:: Fields
initialFields	= Fields { albums   = emptyAlbumTree
			 , config   = emptyConfig
			 , options  = []
			 , status   = Nothing
			 }

-- ------------------------------------------------------------

type AState a = StateT Fields IO a

type Selector a = (AState a, a -> AState ())

-- ------------------------------------------------------------

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

getFrom :: Selector a -> (a -> b) -> AState b
getFrom s f
    = do
      r <- fst s
      return (f r)

set :: Selector a -> a -> AState ()
set = snd

update :: Selector a -> (a -> a) -> AState ()
update (g, s) f = do v <- g
		     s (f v)

io :: IO a -> AState a
io = CMS.liftIO

-- ------------------------------------------------------------

theAlbums :: Selector AlbumTree
theAlbums = (CMS.gets albums, \x -> CMS.modify (\vs -> vs {albums = x}))
 
theConfig :: Selector Config
theConfig = (CMS.gets config, \x -> CMS.modify (\vs -> vs {config = x}))
 
theOptions :: Selector Options
theOptions = (CMS.gets options, \x -> CMS.modify (\vs -> vs {options = x}))

theStatus :: Selector Status
theStatus = (CMS.gets status, \x -> CMS.modify (\vs -> vs {status = x}))

theOption	:: Name -> Selector Value
theOption k	= (lookup1 k, \ v os -> addEntry k v os) `sub` theOptions

theAlbum	:: Path -> Selector Pic
theAlbum p	= (getPic p, \ v ad -> setPic p v ad) `sub` theAlbums


-- ------------------------------------------------------------

perf	:: String -> AState (Maybe a) -> AState ()
perf msg action
    = do
      ok <- getFrom theStatus isNothing
      if ok
	 then do
	      r <- action
	      if isNothing r
		 then
		 set theStatus (Just msg)
		 else
		 return ()
	 else return ()

      
-- ------------------------------------------------------------

runApp	:: AState () -> IO ()
runApp app
	   = do
	     runStateT app initialFields
	     return ()

-- ------------------------------------------------------------

data Cmd = Cmd { cmdLog :: String
	       , cmdActio :: AState ()
	       }

type CmdArrow b c	= IOStateArrow Fields b c

runCmdArrow	:: CmdArrow a b -> AState ()
runCmdArrow ca
    = do
      s0 <- CMS.get
      (s1', _res) <- io $ runIOSLA ca (initialState s0) $ undefined
      CMS.put (xio_userState s1')
      return ()
      

-- ------------------------------------------------------------
