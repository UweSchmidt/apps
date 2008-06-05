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
      cmds  <- getCmd prompt
      if null cmds
	 then return state
	 else do
	      newState <- runCmds cmds state
	      cmdLoop newState
    where
    prompt = ("photo2@" ++) . (++ "> ") . mkAbsPath . joinPath . cwd $ state
    runCmds [] s0
	= return s0
    runCmds (c:cs) s0
	= do
	  s1 <- c s0
	  runCmds cs s1

getCmd	:: String -> IO [Cmd]
getCmd prompt
    = do
      line <- readCmdLine prompt
      return $ uncurry parseCmd (scanLine line)

readCmdLine	:: String -> IO String
readCmdLine prompt
    = do
      line <- readline prompt
      let line' = stringTrim . fromMaybe "" $ line
      CM.when (length line' > 1) (addHistory line')
      if null line'
	 then readCmdLine prompt
	 else return line'

mkCmd	= return . runCmd
mkCmd' io = return . runCmd' io

liftCmd	:: Monad m => IO a -> m Cmd
liftCmd c = return $
	    ( \ s -> do
		     c
		     return s )

parseCmd	:: String -> [String] -> [Cmd]
parseCmd "open" [archive]
    = mkCmd ( loadArchiveAndConfig archive
	      >>>
	      rootWd
	      >>>
	      withCwd loadAlbums
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
parseCmd "cd"  args	= parseCd  args

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

parseCmd "exit" _	= fail ""
parseCmd "q" _		= fail ""

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

parseLs ps
    | null ps
      ||
      n == rootPath
	= mkLs withCwd
    | null ps'
      &&
      isAbsPath n
	= mkLs $ withAbsDir p'
    | null ps'
	= mkLs $ withDir p
    | otherwise
	= illegalCmd "ls" ps
    where
    mkLs wd     = mkCmd ( get theAlbums
			  >>> wd getAlbumPaths
			  >>> arrIO printPath
			)


    (n : ps')   = ps
    p@(_:p')    = splitPath n
    printPath s = putStrLn (mkAbsPath . joinPath $ s)

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

parseCd []
    = mkCmd ( rootWd )

-- ------------------------------------------------------------
