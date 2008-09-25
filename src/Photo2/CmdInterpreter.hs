module Photo2.CmdInterpreter
where

import qualified Control.Monad as CM

import           Data.List
import           Data.Maybe
import qualified Data.Map as M

import           Photo2.ArchiveTypes
import           Photo2.Arrow
import           Photo2.Html
import           Photo2.FilePath

import           System.IO
import           System.Console.Readline
                 ( readline
		 , addHistory
		 )

import           Text.XML.HXT.Arrow

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

mkCmd			:: CmdArrow a b -> [Cmd]
mkCmd			= return . runCmd

mkCmd'			:: (Monad m) => CmdArrow a b -> ([b] -> IO ()) -> m Cmd
mkCmd' io		= return . runCmd' io

liftCmd			:: Monad m => IO a -> m Cmd
liftCmd c		= return $
			  \ s -> ( do
				   c
				   return s )

parseCmd		:: String -> [String] -> [Cmd]
parseCmd "open" []	= parseCmd "open" ["archive.xml"]

parseCmd "open" [archive]
    = mkCmd ( loadArchiveAndConfig archive
	      >>>
	      rootWd
	      >>>
	      withCwd loadAlbums
	    )

parseCmd "close" []
    = mkCmd ( withRootDir storeAllChangedAlbums
	      >>>
	      storeConfig
	      >>>
	      storeArchive
	    )
      ++
      parseCd []

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

parseCmd "defpicattr" [n,v]
    = mkCmd ( changeComp theConfigPicAttrs (addEntry v n) )

parseCmd "pwd" []
    = mkCmd ( get theWd
	      >>>
	      arrIO (putStrLn . (rootPath </>) . joinPath)
	    )

parseCmd "ls"        args	= parseLs        args
parseCmd "lsr"       args	= parseLsr       args
parseCmd "lsra"      args	= parseLsra      args
parseCmd "edited"    args	= parseEdited    args
parseCmd "cat"       args	= parseCat       args
parseCmd "dump"      args	= parseDump      args
parseCmd "relatives" args	= parseRelatives args
parseCmd "store"     args	= parseStore     args
parseCmd "storepics" args	= parseStorePics args
parseCmd "update"    args	= parseUpdate    args
parseCmd "newattrs"  args	= parseNewAttrKeys	args

parseCmd "attr"      args
    | length args >=2		= parseAttr		args

parseCmd "deleteattr" args
    | length args ==2		= parseDeleteAttr	args

parseCmd "rename"    args
    | length args == 2		= parseRename
				  "rename" (renamePic (concat . drop 1 $ args))
				  args

parseCmd "rename-cont"    args
    | length args <= 1		= parseRename
				  "rename-cont"  renameContent
				  args

parseCmd "gen-html"  args
    | length args <=2		= parseGenHtml          args

parseCmd "find"      args
    | length args `elem` [1..3]	= parseFind		args

parseCmd "cd"        args	= parseCd		args

parseCmd "xxx" args		= parseTest             args

parseCmd "?" []
    = liftCmd $
      hPutStrLn stdout usage
    where
    usage = unlines $
	    [ "commands available from the promt:"
	    , ""
	    , "  ?                          help (this text)"
	    , "  attr path n [vl]           set attribute value for picture/album or unset attr, if vl is missing"
	    , "  deleteattr path pattern    delete all attributes matching the attribute name pattern"
	    , "  cat [path]                 list the contents of an entry, default is current working album"
	    , "  close                      write the whole data, albums, config and archive"
	    , "  config                     list config data"
	    , "  dump [path]                list the contents of a whole album, default is current working album"
	    , "  edited [path]              list all edited pictures"
	    , "  exit,q                     exit photo2"
	    , "  find path kre vre          list all pictures with matching attribute key and value"
	    , "  gen-html [p] [f]           generate HTML pages, default album is current album, default format \"html-1024x768\""
	    , "  ls [path]                  list album and picture names, default is the current working album"
	    , "  lsr [path]                 list album and picture names recursively, default is the current working album"
	    , "  lsra [path]                load and list album and picture names recursively, default is the current working album"
	    , "  newattrs [path]            change attribute keys to new format"
	    , "  open <archive>             load a photo archive, configuration and root album"
	    , "  options                    list options"
	    , "  pwd                        print working album (dir)"
	    , "  relatives [path]           list the paths of the parent, the previous and the next entry"
	    , "  rename-cont [path]         rename all pictures in an album"
	    , "  rename path newid          rename picture"
	    , "  set <opt> [val]            set or overwrite an option, default value is \"1\""
	    , "      copy-copy              force creation of all copies in all required sizes"
	    , "      copy-exif              force import of exif info from original"
	    , "      copy-org               force import of original images"
	    , "      debug                  debug output"
	    , "  defpicattr a val           define a picture attribute"
	    , "  store-config               write the config data"
	    , "  store [path]               write all albums addressed by path and unload subalbums"
	    , "  storepics [path]           write all pictures and albums addressed by path and unload subalbums"
	    , "  unset <opt>                unset an option"
	    , "  update [path]              import image and update copies, if original has changed"
	    , "  version                    print photo2 version"
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
parseWdCmd pa name ps
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
    mkWdCmd wd  = mkCmd $ wd (\ p'' -> pa p'' `orElse` cFailed)
    cFailed	= perform (arrIO0 $ hPutStrLn stderr ("command failed: " ++ unwords (name : ps)))
		  >>>
		  none
    (n : ps')   = ps
    p@(_:p')    = splitPath n


parseWdCmd'	:: PathArrow AlbumTree b -> String -> [String] -> [Cmd]
parseWdCmd' pa	= parseWdCmd (loadAndCheckAlbum />>>/ pa)

-- ------------------------------------------------------------

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

parseLs		:: [String] -> [Cmd]
parseLs		= parseLs' (getTreeAndProcessChildren constA) "ls"

parseLsr	:: [String] -> [Cmd]
parseLsr	= parseLs' (getTreeAndProcessDesc constA) "lsr"

parseLsra	:: [String] -> [Cmd]
parseLsra	= parseLs' (getTreeAndProcessDescC constA) "lsra"

parseEdited	:: [String] -> [Cmd]
parseEdited	= parseLs' ( getTreeAndProcessDesc $
			     \ p -> entryEdited `guards` constA p
			   ) "edited"

parseLs'		:: PathArrow AlbumTree Path -> String -> [String] -> [Cmd]
parseLs' pa ps		= parseWdCmd' ls ps
                          where
			  ls	= ( pa
				    />>>/
				    const (arrIO (putStrLn . mkAbsPath . joinPath))
				  )
				  `withDefaultRes` ()

parseCat 		:: [String] -> [Cmd]
parseCat		= parseWdCmd' cat "cat"
                        where
			cat = (getTreeAndProcess (\ p -> constA p &&& getNode))
			      />>>/
			      const (xpickleDocument xpAlbumEntry [ (a_indent, v_1)
								  , (a_no_xml_pi, v_1)
								  ] ""
				    )

parseGenHtml		:: [String] -> [Cmd]
parseGenHtml []		= parseGenHtml [".", "html-1024x768"]
parseGenHtml [p]	= parseGenHtml (p : ["html-1024x768"])
parseGenHtml (p:f:_)	= parseWdCmd' gen "gen-html" [p]
			  where
			  gen = getTreeAndProcess (withConfig (genHtml f))

parseDump		:: [String] -> [Cmd]
parseDump		= parseWdCmd' dump "dump"
                          where
			  dump = getTree
				 />>>/
				 const (xpickleDocument xpAlbumTree [ (a_indent, v_1)
								    , (a_no_xml_pi, v_1)
								    ] ""
				       )

parseRelatives		:: [String] -> [Cmd]
parseRelatives		= parseWdCmd' relatives "relatives"
                          where
			  relatives
			      = getRelatives
				/>>>/
				(\ p -> arrIO ( \ (parent, prev, next) ->
						putStrLn ( "this     = " ++ fp p      ++ "\n" ++
							   "parent   = " ++ fp parent ++ "\n" ++
							   "previous = " ++ fp prev   ++ "\n" ++
							   "next     = " ++ fp next
							 )
					      )
				)
			  fp []	= ""
			  fp p	= mkAbsPath . joinPath $ p

parseStore		:: [String] -> [Cmd]
parseStore		= parseWdCmd' storeAllChangedAlbums "store"

parseStorePics		:: [String] -> [Cmd]
parseStorePics		= parseWdCmd' storeAllChangedEntries "storepics"

parseTest		:: [String] -> [Cmd]
parseTest		= parseWdCmd' test "xxx"
                          where
			  test = changeAlbums $
				 processTreeSelfAndDesc ( const $ perform $
							  getNode >>> arr picId >>> arrIO print
							)

parseUpdate		:: [String] -> [Cmd]
parseUpdate		= parseWdCmd' (changeAlbums update) "update"
                          where
			  update = processTreeSelfAndDesc
				   ( withConfig updatePic
				     />>>/
				     withConfig updateExifAttrs
				   )

parseNewAttrKeys	:: [String] -> [Cmd]
parseNewAttrKeys	= parseWdCmd' ( changeAlbums $
				        processTreeSelfAndDesc (withConfig updateAttrKeys)
				      ) "newattrs"

parseRename		:: String -> ConfigArrow AlbumTree AlbumTree -> [String] -> [Cmd]
parseRename cn ca al	= parseWdCmd' ( changeAlbums $
				        processTree (withConfig ca)
				      ) cn (take 1 al)

parseAttr		:: [String] -> [Cmd]
parseAttr al		= parseWdCmd' ( changeAlbums $
					processTree (withConfig (updateAttr an (unwords avl)))
				      ) "attr" (take 1 al)
                          where
			  (an : avl) = tail al

parseDeleteAttr		:: [String] -> [Cmd]
parseDeleteAttr al	= parseWdCmd' ( changeAlbums $
					processTreeSelfAndDesc (deleteAttr ap)
				      ) "deleteattr" (take 1 al)
                          where
			  (ap : _) = tail al

-- TODO refactor

parseFind		:: [String] -> [Cmd]
parseFind al		= parseWdCmd fe "find" (take 1 al)
                          where
			  rek	= concat . take 1 . drop 1 $ al
			  rev   = concat          . drop 2 $ al
			  fe	= findEntries loadAlbums (getAllWithAttr rek rev) puts
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
	    = loadAndCheckAlbum p
	      >>>
	      ( constA p >>> set theWd )
	where
	p = normalPath p0

-- ------------------------------------------------------------
