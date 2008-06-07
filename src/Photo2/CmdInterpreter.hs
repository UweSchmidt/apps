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

mkCmd		:: CmdArrow a b -> [Cmd]
mkCmd		= return . runCmd

mkCmd'		:: (Monad m) => CmdArrow a b -> ([b] -> IO ()) -> m Cmd
mkCmd' io	= return . runCmd' io

liftCmd		:: Monad m => IO a -> m Cmd
liftCmd c	= return $
		  ( \ s -> do
		           c
		           return s )

parseCmd	:: String -> [String] -> [Cmd]
parseCmd "open" []
    = parseCmd "open" ["archive.xml"]

parseCmd "open" [archive]
    = mkCmd ( loadArchiveAndConfig archive
	      >>>
	      rootWd
	      >>>
	      withCwd loadAlbums
	    )

parseCmd "config" []
    = mkCmd ( get theConfig
	      >>>
	      xpickleDocument xpConfig [ (a_indent, v_1)
				       , (a_no_xml_pi, v_1)
				       ] ""
	    )

parseCmd "options" []
    = mkCmd ( get theOptions
	      >>>
	      arrIO dumpOptions
	    )
    where
    dumpOptions
	= putStrLn . unlines . map (\ (n,v) -> n ++ "\t= " ++ v)

parseCmd "set" [n]
    = parseCmd "set" [n,v_1]

parseCmd "set" [n,v]
    = mkCmd ( changeComp theOptions (addEntry n v) )

parseCmd "unset" [n]
    = mkCmd ( changeComp theOptions (delEntry n) )

parseCmd "pwd" []
    = mkCmd ( get theWd
	      >>>
	      arrIO (putStrLn . (rootPath </>) . joinPath)
	    )

parseCmd "ls"   args	= parseLs   args
parseCmd "ls-r" args	= parseLsr  args
parseCmd "cat"  args	= parseCat  args
parseCmd "dump" args	= parseDump args
parseCmd "cd"   args	= parseCd   args

parseCmd "?" []
    = liftCmd $
      hPutStrLn stdout usage
    where
    usage = unlines $
	    [ "commands available from the promt:"
	    , ""
	    , "  ?                  help (this text)"
	    , "  open <archive>     load a photo archive, configuration and root album"
	    , "  config             list config data"
	    , "  options            list options"
	    , "  set <opt> [val]    set or overwrite an option, default value is \"1\""
	    , "      debug 1        debug output"
	    , "  unset <opt>        unset an option"
	    , "  ls [path]          list album and picture names, default is the current working album"
	    , "  ls-r [path]        list album and picture names recursively, default is the current working album"
	    , "  cat [path]         list the contents of an entry, default is current working album"
	    , "  dump [path]        list the contents of a whole album, default is current working album"
	    , "  pwd                print working album (dir)"
	    , "  version            print photo2 version"
	    , "  exit,q             exit photo2"
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

illegalCmd	:: (Monad m) => String -> [String] -> m Cmd
illegalCmd c args
    = liftCmd $
      hPutStrLn stderr ( "unknown command or wrong arguments: " ++ unwords (c : args) ++ "\n" ++
			 "try ? for help"
		       )

-- ------------------------------------------------------------
--
-- execute a comand with one argument, a path for addressing one node
-- if the path is empty, the currend working dir is taken,
-- if it's a relative path, it's adressed via current working dir

parseWdCmd :: (Path -> CmdArrow a b)
              -> String
              -> [String]
              -> [Cmd]

parseWdCmd action name ps
    | null ps
	= mkWdCmd withCwd
    | not . null $ ps'
	= illegalCmd name ps
    | n == rootPath
	= mkWdCmd $ withRootDir
    | isAbsPath n
	= mkWdCmd $ withAbsDir p'
    | otherwise
	= mkWdCmd $ withDir p
    where
    mkWdCmd wd  = mkCmd $ wd (\ x -> action x `orElse` cFailed)
    cFailed	= perform (arrIO0 $ hPutStrLn stderr ("command failed: " ++ unwords (name : ps)))
		  >>>
		  none
    (n : ps')   = ps
    p@(_:p')    = splitPath n


-- ------------------------------------------------------------

parseLs :: [String] -> [Cmd]
parseLs
    = parseWdCmd (getLsPaths getAlbumPaths) "ls"

parseLsr :: [String] -> [Cmd]
parseLsr
    = parseWdCmd (getLsPaths getAllAlbumPaths) "ls-r"

getLsPaths	:: (Path -> CmdArrow AlbumTree Path) -> Path -> CmdArrow a ()
getLsPaths gt p
    = loadAlbums p
      >>>
      gt p
      >>>
      arrIO ( putStrLn . mkAbsPath . joinPath )

-- ------------------------------------------------------------

parseCat :: [String] -> [Cmd]
parseCat
    = parseWdCmd getEntry "cat"
    where
    getEntry p
	= loadAlbums p
	  >>>
	  getAlbumEntry p
	  >>>
	  xpickleDocument xpAlbumEntry [ (a_indent, v_1)
			               , (a_no_xml_pi, v_1)
			               ] ""

parseDump	:: [String] -> [Cmd]
parseDump
    = parseWdCmd dumpEntry "dump"
    where
    dumpEntry p
	= loadAlbums p
	  >>>
	  getTreeByPath p
	  >>>
	  xpickleDocument xpAlbumTree [ (a_indent, v_1)
				      , (a_no_xml_pi, v_1)
				      ] ""

-- ------------------------------------------------------------

parseCd :: [String] -> [Cmd]
parseCd []
    = parseCd [rootPath]

parseCd ps
    = parseWdCmd changeDir "cd" ps
    where
    changeDir p0
	| null p	-- above the top of the roof
	    = none
	| otherwise
	    = loadAlbums p
	      >>>
	      checkPath p
              >>>
	      ( constA p >>> set theWd )
	where
	p = normalPath p0

-- ------------------------------------------------------------
