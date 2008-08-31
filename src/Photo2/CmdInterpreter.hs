module Photo2.CmdInterpreter
where

import qualified Control.Monad as CM

import           Data.List
import           Data.Maybe
import qualified Data.Map as M

import           Photo2.ArchiveTypes
import           Photo2.Arrow
import           Photo2.FilePath

import           System.IO
import           System.Console.Readline
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

parseCmd "close" []
    = mkCmd ( withRootDir storeAllAlbums
	      >>>
	      storeConfig
	      >>>
	      storeArchive
	    )

parseCmd "config" []
    = mkCmd ( get theConfig
	      >>>
	      xpickleDocument xpConfig [ (a_indent, v_1)
				       , (a_no_xml_pi, v_1)
				       ] ""
	    )

parseCmd "options" []
    = mkCmd ( get theConfigAttrs
	      >>>
	      arrIO dumpOptions
	    )
    where
    dumpOptions
	= putStrLn . unlines . map (\ (n,v) -> n ++ "\t= " ++ v) . M.toList

parseCmd "set" [n]
    = parseCmd "set" [n,v_1]

parseCmd "set" [n,v]
    = mkCmd ( changeComp theConfigAttrs (M.insert n v) )

parseCmd "unset" [n]
    = mkCmd ( changeComp theConfigAttrs (M.delete n) )

parseCmd "pwd" []
    = mkCmd ( get theWd
	      >>>
	      arrIO (putStrLn . (rootPath </>) . joinPath)
	    )

parseCmd "ls"        args	= parseLs        args
parseCmd "lsr"       args	= parseLsr       args
parseCmd "lsar"      args	= parseLsra      args
parseCmd "edited"    args	= parseEdited    args
parseCmd "cat"       args	= parseCat       args
parseCmd "dump"      args	= parseDump      args
parseCmd "relatives" args	= parseRelatives args
parseCmd "store"     args	= parseStore     args
parseCmd "update"    args	= parseUpdate    args
parseCmd "newattrs"  args	= parseNewAttrKeys	args
parseCmd "attr"      args
    | length args >=2		= parseAttr		args
parseCmd "find"  args
    | length args `elem` [1..3]	= parseFind		args
parseCmd "cd"        args	= parseCd		args

parseCmd "?" []
    = liftCmd $
      hPutStrLn stdout usage
    where
    usage = unlines $
	    [ "commands available from the promt:"
	    , ""
	    , "  ?                  help (this text)"
	    , "  open <archive>     load a photo archive, configuration and root album"
	    , "  close              write the whole data, albums, config and archive"
	    , "  config             list config data"
	    , "  options            list options"
	    , "  set <opt> [val]    set or overwrite an option, default value is \"1\""
	    , "      debug          debug output"
	    , "      copy-org       force import of original images"
	    , "      copy-exif      force import of exif info from original"
	    , "      create-copy    force creation of all copies in all required sizes"
	    , "  unset <opt>        unset an option"
	    , "  ls [path]          list album and picture names, default is the current working album"
	    , "  lsr [path]         list album and picture names recursively, default is the current working album"
	    , "  lsar [path]        load and list album and picture names recursively, default is the current working album"
	    , "  edited [path]      list all edited pictures"
	    , "  find path kre vre  list all pictures with matching attribute key and value"
	    , "  cat [path]         list the contents of an entry, default is current working album"
	    , "  dump [path]        list the contents of a whole album, default is current working album"
	    , "  relatives [path]   list the paths of the parent, the previous and the next entry"
	    , "  update [path]      import image and update copies, if original has changed"
	    , "  newattrs [path]    change attribute keys to new format"
	    , "  store [path]       write all albums addressed by path and unload subalbums"
	    , "  attr path n vl     set attribute value for picture/album selected by path"
	    , "  store-config       write the config data"
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

parseWdCmd	:: PathArrow a b -> String -> [String] -> [Cmd]
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
    = parseWdCmd (listEntries loadAlbums getAlbumPaths) "ls"

parseLsr :: [String] -> [Cmd]
parseLsr
    = parseWdCmd (listEntries loadAlbums getAllAlbumPaths) "lsr"

parseLsra :: [String] -> [Cmd]
parseLsra
    = parseWdCmd (listEntries loadAllAlbums getAllAlbumPaths) "lsar"

parseEdited :: [String] -> [Cmd]
parseEdited
    = parseWdCmd (listEntries loadAlbums getAllEditedPaths) "edited"

listEntries	:: PathArrow a AlbumTree ->
		   PathArrow AlbumTree Path ->
		   PathArrow a ()
listEntries ld gt
		= findEntries ld gt (putStrLn . mkAbsPath . joinPath)

findEntries	:: PathArrow a AlbumTree ->
		   PathArrow AlbumTree b ->
		   (b -> IO ()) ->
		   PathArrow a ()
findEntries ld gt out p
    = ( ld p
	>>>
	gt p
	>>>
	arrIO out
      ) `withDefault` ()

-- ------------------------------------------------------------

parseCat :: [String] -> [Cmd]
parseCat
    = parseWdCmd cat "cat"
    where
    cat p
	= loadAlbums p
	  >>>
	  getAlbumEntry p
	  >>>
	  xpickleDocument xpAlbumEntry [ (a_indent, v_1)
			               , (a_no_xml_pi, v_1)
			               ] ""

parseDump	:: [String] -> [Cmd]
parseDump
    = parseWdCmd dump "dump"
    where
    dump p
	= loadAlbums p
	  >>>
	  getTreeByPath p
	  >>>
	  xpickleDocument xpAlbumTree [ (a_indent, v_1)
				      , (a_no_xml_pi, v_1)
				      ] ""

parseRelatives	:: [String] -> [Cmd]
parseRelatives
    = parseWdCmd relatives "relatives"
    where
    relatives p
	= loadAlbums p
	  >>>
	  getRelatives p
	  >>>
	  arrIO ( \ (parent, prev, next)
		  -> putStrLn ( "this     = " ++ fp p      ++ "\n" ++
				"parent   = " ++ fp parent ++ "\n" ++
				"previous = " ++ fp prev   ++ "\n" ++
				"next     = " ++ fp next
			      )
		)
    fp []	= ""
    fp p	= mkAbsPath . joinPath $ p

parseStore	:: [String] -> [Cmd]
parseStore
    = parseWdCmd storeAllAlbums "store"

parseUpdate	:: [String] -> [Cmd]
parseUpdate
    = parseWdCmd update "update"
    where
    update p
	= loadAlbums p
	  >>>
	  updateAllPics p
	  >>>
	  set theAlbums

parseNewAttrKeys	:: [String] -> [Cmd]
parseNewAttrKeys
    = parseWdCmd update "newattrs"
    where
    update p
	= loadAlbums p
	  >>>
	  updateAllAttrKeys p
	  >>>
	  set theAlbums

parseAttr	:: [String] -> [Cmd]
parseAttr al
    = parseWdCmd attr "attr" (take 1 al)
    where
    (an : avl) = tail al
    attr p
	= loadAlbums p
	  >>>
	  updateAttr an (unwords avl) p
	  >>>
	  set theAlbums

parseFind	:: [String] -> [Cmd]
parseFind al
    = parseWdCmd find "find" (take 1 al)
    where
    rek	= concat . take 1 . drop 1 $ al
    rev = concat          . drop 2 $ al
    find
	= findEntries loadAlbums (getAllWithAttr rek rev) puts
    puts (p, k, v)
	= putStrLn ( intercalate ": "
		     [ mkAbsPath . joinPath $ p
		     , k , v
		     ]
		   )

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
