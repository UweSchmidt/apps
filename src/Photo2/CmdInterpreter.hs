module Photo2.CmdInterpreter
where

import Photo2.ArchiveTypes
import Photo2.Arrow
import Photo2.FilePath

import qualified Control.Monad as CM

import Data.Maybe

import System.IO
import System.Console.Readline
    ( readline
    , addHistory
    )

import Text.XML.HXT.Arrow

type Cmd = AppState -> IO AppState

m1 = do
     s1 <- cmdLoop initialAppState
     return ()

cmdLoop	:: Cmd
cmdLoop state
    = do
      cmd  <- getCmd
      if isNothing cmd
	 then return state
	 else do
	      newState <- (fromJust cmd) state
	      cmdLoop newState

getCmd	:: IO (Maybe Cmd)
getCmd
    = do
      line <- readCmdLine
      return $ uncurry parseCmd (scanLine line)

prompt	:: String
prompt	= "photo2 > "

readCmdLine	:: IO String
readCmdLine
    = do
      line <- readline prompt
      let line' = stringTrim . fromMaybe "" $ line
      CM.when (length line' > 1) (addHistory line')
      if null line'
	 then readCmdLine
	 else return line'

mkCmd	= Just . runCmd
mkCmd' io = Just . runCmd' io

liftCmd	:: IO a -> Maybe Cmd
liftCmd c = Just $
	    ( \ s -> do
		     c
		     return s )

parseCmd	:: String -> [String] -> (Maybe Cmd)
parseCmd "open" [archive]
    = mkCmd ( loadArchiveAndConfig archive
	      >>>
	      loadRootAlbum
	    )

parseCmd "albums" []
    = mkCmd ( get theAlbums
	      >>>
	      xpickleDocument xpAlbumTree [ (a_indent, v_1)
					  , (a_no_xml_pi, v_1)
					  ] ""
	    )

parseCmd "config" []
    = mkCmd ( get theConfig
	      >>>
	      xpickleDocument xpConfig [ (a_indent, v_1)
				       , (a_no_xml_pi, v_1)
				       ] ""
	    )

parseCmd "entry" []
    = parseCmd "entry" [rootPath]

parseCmd "entry" [p]
    = mkCmd
      ( get theAlbums
	>>>
	getAlbumEntry (tail $ splitPath p)
	>>>
	arrIO print
      )
	
parseCmd "ls"  args	= parseLs  args
parseCmd "cat" args	= parseCat args

parseCmd "?" []
    = liftCmd $
      hPutStrLn stdout usage
    where
    usage = unlines $
	    [ "commands available from the promt:"
	    , ""
	    , "\t?\t\thelp (this text)"
	    , "\topen <archive>\tload a photo archive, configuration and root album"
	    , "\tdump\t\tXML output of root album"
	    , "\tls\t[path]\tlist album and picture names, default is the root"
	    , "\tcat\t[path]\tlist the contents of an entry, default is the root"
	    , "\tversion\t\tprint photo2 version"
	    , "\texit,q\t\texit photo2"
	    ]

parseCmd "version" []
    = liftCmd $
      hPutStrLn stdout "Photo2 version 0.0 from 2008-06-05"

parseCmd "exit" _	= Nothing

parseCmd "q" _		= Nothing

parseCmd c args
    = illegalCmd c args

-- ------------------------------------------------------------

scanLine	:: String -> (String, [String])
scanLine s
    | null args = ("",[])
    | otherwise	= (head args, tail args)
    where
    args = words s

-- ------------------------------------------------------------

illegalCmd c args
    = liftCmd $
      hPutStrLn stderr ( "unknown command or wrong arguments: " ++ unwords (c : args) ++ "\n" ++
			 "try ? for help"
		       )

-- ------------------------------------------------------------

parseLs []
    = parseLs [rootPath]

parseLs [n]
    | n == rootPath
	= mkCmd ( getRootPath >>> arrIO printPath )
    | isAbsPath n
	= mkCmd ( get theAlbums >>> getAlbumPaths p >>> arrIO printPath )
    | otherwise
	= parseLs [mkAbsPath n]	-- preliminary: no current album concept
    where
    (_ : p) = splitPath n
    printPath s = putStrLn (mkAbsPath . joinPath $ s)

parseLs args
    = illegalCmd "ls" args

-- ------------------------------------------------------------

parseCat []
    = parseCat [rootPath]

parseCat [n]
    | isAbsPath n
	= mkCmd ( get theAlbums
		  >>>
		  getEntry
		  >>>
		  xpickleDocument xpAlbumEntry [ (a_indent, v_1)
					       , (a_no_xml_pi, v_1)
					       ] ""
		)
    | otherwise
	=  parseCat [mkAbsPath n]
    where
    getEntry
	| n == rootPath
	    = getAlbumEntry $< getRootPath
	| otherwise
	    =  getAlbumEntry (tail $ splitPath n)

parseCat args
    = illegalCmd "cat" args

-- ------------------------------------------------------------
