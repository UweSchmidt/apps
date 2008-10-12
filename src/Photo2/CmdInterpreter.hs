module Photo2.CmdInterpreter
where

import qualified Control.Monad as CM

import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Map as M

import           Photo2.ArchiveTypes
import           Photo2.Arrow
import           Photo2.Html
import           Photo2.FilePath
import           Photo2.ImportDialog

import           System.IO
import           System.Console.Readline
                 ( readline
		 , addHistory
		 )

import           Text.XML.HXT.Arrow
-- import           Text.XML.HXT.DOM.UTF8Decoding
import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

-- ------------------------------------------------------------

tokenizeCmdLine :: String -> Either String [String]
tokenizeCmdLine	= checkTokens . tokenize' tokens

checkTokens :: [Either String String] -> Either String [String]
checkTokens ts
    | tokensOk	= Right . map unescape . filter ( not . all isSpace) . concatMap (either (const []) (:[])) $ ts
    | otherwise = Left . concatMap (either id id) $ ts
    where
    tokensOk = all (either (const False) (const True)) $ ts

    unescape s@('\"' : _)	= filter (/= '\\') . init . tail $ s
    unescape s			= s

tokens	:: String
tokens	= intercalate "|" . map par $ toks
          where
	  toks = [ "[^ \\n\\t\\r;" ++ q ++ bsbs ++ "]+"
		 , q ++ "([^" ++ bsbs ++ q ++ "]|(" ++ bsbs ++ ".))*" ++ q
		 , ";"
		 , "[ \\n\\t\\r]+"
		 ]
	  par s	= "(" ++ s ++ ")"
	  bsbs	= bs ++ bs
	  q     = "\""
	  bs    = "\\"

parseCmdLine	:: Either String [String] -> [Cmd]
parseCmdLine (Left line)	= illegalCmd "" [line]
parseCmdLine (Right ts)		= concatMap parseCmd' . splitCmds $ ts
				  where
				  parseCmd' []		= []
				  parseCmd' (c:args)	= parseCmd c args

				  splitCmds []		= [[]]
				  splitCmds (";" : rs)	= [] : splitCmds rs
				  splitCmds (c : rs)	= let (cs:rss) = splitCmds rs in (c:cs):rss

-- ------------------------------------------------------------

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
      return . parseCmdLine . tokenizeCmdLine $ line

readCmdLine	:: String -> IO String
readCmdLine prompt
    = do
      line <- readline prompt
      let line' = stringTrim {- . fst . decodeUtf8 -} . fromMaybe "" $ line
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

-- ------------------------------------------------------------

align	:: [String] -> [String]
align xs
    = map ( take maxLen . (++ replicate maxLen ' ')) $ xs
    where
    maxLen = maximum . (0:) . map length $ xs

fmtTable	:: String -> [(String, String)] -> [String]
fmtTable del ts
    = zipWith (\ x y -> x ++ del ++ y)
              (align . map fst $ ts)
	      (map snd $ ts)

-- ------------------------------------------------------------

parseCmd						:: String -> [String] -> [Cmd]
parseCmd c@"open" []					= parseCmd c ["archive2.xml"]
parseCmd   "open" [archive]				= mkCmd ( loadArchiveAndConfig archive
								  >>>
								  rootWd
								  >>>
								  withCwd loadAlbums
								)
parseCmd "config" []					= mkCmd ( get theConfig
								  >>>
								  xpickleDocument xpConfig [ (a_indent, v_1)
											   , (a_no_xml_pi, v_1)
											   , (a_output_encoding, usAscii)
											   ] ""
								)
parseCmd "options" []					= mkCmd ( get theConfigAttrs
								  >>>
								  arrIO dumpOptions
								)
                                                          where
							  dumpOptions = putStrLn . unlines . fmtTable " = " . M.toList

parseCmd c@"set" [n]					= parseCmd c [n,v_1]
parseCmd   "set" [n,v]					= mkCmd ( changeComp theConfigAttrs (M.insert n v) )
parseCmd "unset" [n]					= mkCmd ( changeComp theConfigAttrs (M.delete n) )
parseCmd "defpicattr" [n,v]				= mkCmd ( changeComp theConfigPicAttrs (addEntry v n) )
parseCmd "pwd" []					= mkCmd ( get theWd
								  >>>
								  arrIO (putStrLn . (rootPath </>) . joinPath)
								)
parseCmd c@"ls"            args				= parseLs' (getTreeAndProcessChildren constA) c args
parseCmd c@"ls-all"        args				= parseLs' (getTreeAndProcessDesc constA)     c args
parseCmd c@"ls-rec"        args				= parseLs' (getTreeAndProcessDescC constA)    c args
parseCmd c@"ls-mod"        args				= parseLs' (getTreeAndProcessSelfAndDesc $
								    \ p -> entryEdited `guards` constA p
								   )                                  c args
parseCmd c@"cat"           args				= parseCat                                    c args
parseCmd c@"dump"          args				= parseDump                                   c args

parseCmd "relatives" args	= parseRelatives args
parseCmd "load"      args	= parseLoad      args

parseCmd c@"store"         args				= parseStore ""                               c args
parseCmd c@"store-picture" args				= parseStore "picture"                        c args
parseCmd c@"store-album"   args				= parseStore "album"                          c args
parseCmd   "store-config" []				= mkCmd storeConfig
parseCmd   "close"        []				= mkCmd ( withRootDir (withConfig (storeAll ""))
								  >>>
								  storeConfig
								  >>>
								  storeArchive
								)
							  ++
							  parseCd []


parseCmd c@"update"        args				= parseUpdate False                           c args
parseCmd c@"update-all"    args				= parseUpdate True                            c args

parseCmd "newattrs"  args	= parseNewAttrKeys	args

parseCmd c@"attr"          args	| length args >= 2	= parseAttr c args
parseCmd c@"deleteattr"    args	| length args == 2	= parseDeleteAttr c args
parseCmd c@"import"        args	| length args <= 1	= parseImport c args
parseCmd c@"newalbum"      args	| length args == 2	= parseModifiy c (newAlbum (concat . drop 1 $ args)) args
parseCmd c@"setalbumpic"   args	| length args == 2	= parseModifiy c (setAlbumPic (concat . drop 1 $ args)) args
parseCmd c@"rename"        args	| length args == 2	= parseModifiy c (renamePic (concat . drop 1 $ args)) args
parseCmd c@"rename-cont"   args	| length args <= 1	= parseModifiy c renameContent args
parseCmd c@"html"          args	| length args <= 2	= parseGenHtml c False args
parseCmd c@"html-all"      args | length args <= 2	= parseGenHtml c True args

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
	    , ""
	    , "  open [archive]             load a photo archive, configuration and root album, default is \"archive2.xml\""
	    , "  load [path]                load all subalbums and pictures"
	    , "  store [path]               write all albums and unload subalbums, format is given by option \"store-format\""
	    , "  store-album   [path]       write all pictures and albums in album format"
	    , "  store-picture [path]       write all pictures and albums in picture format"
	    , "  store-config               write the config data"
	    , "  close                      write the whole data, albums, config and archive"
	    , "  exit,q                     exit photo2"
	    , ""
	    , "  options                    list options"
	    , "  set <opt> [val]            set or overwrite an option, default value is \"1\""
	    , "      copy-copy              force creation of all copies in all required sizes"
	    , "      copy-exif              force import of exif info from original"
	    , "      copy-org               force import of original images"
	    , "      debug                  debug output"
	    , "      import-base            base dir for image import, default is \"../Diakasten\""
	    , "      import-by-date         images are sorted by creation date"
	    , "      import-dir             dir path relative to import-base, default is \".\""
	    , "      import-pattern         only files matching import pattern are imported, default pattern is \".*\""
	    , "      import-since           only files newer than date are imported, default is \"2008-01-01\""
	    , "      layout                 select the layout for html page generation"
	    , "      store-all              force storing entries even if not changed"
	    , "      store-format           \"album\" or \"picture\", default is \"picture\""
	    , "  unset <opt>                unset an option"
	    , "  config                     list whole config data, not only options"
	    , ""
	    , "  pwd                        print working album (dir)"
	    , "  ls     [path]              list album and picture names, default is the current working album"
	    , "  ls-all [path]              list album and picture names recursively, default is the current working album"
	    , "  ls-rec [path]              load and list album and picture names recursively, default is the current working album"
	    , "  ls-mod [path]              list all modified entries"
	    , "  cat    [path]              list the contents of an entry, default is current working album"
	    , "  dump   [path]              list the contents of a whole album, default is current working album"
	    , "  find path kre vre          list all pictures with matching attribute key and value"
	    , "  relatives [path]           list the paths of the parent, the previous and the next entry"
	    , ""
	    , "  newalbum path newid        create a new album within path"
	    , "  setalbumpic path id        set the album picture from the list of pictures within the album"
	    , "  import     [path]          star import dialog for adding new pictures into album"
	    , "  update     [path]          copy original and update copies, if original has changed"
	    , "  update-all [path]          recursively copy originals and update copies, if originals have changed"
	    , ""
	    , "  html     [p] [f]           generate single HTML page, default album is current album, default format \"html-1024x768\""
	    , "  html-all [p] [f]           generate all HTML pages, default album is current album, default format \"html-1024x768\""
	    , ""
	    , "  attr path n [vl]           set attribute value for picture/album or unset attr, if vl is missing"
	    , "  deleteattr path pattern    delete all attributes matching the attribute name pattern"
	    , "  newattrs [path]            change attribute keys to new format"
	    , "  rename-cont [path]         rename all pictures in an album"
	    , "  rename path newid          rename picture"
	    , "  defpicattr a val           define a picture attribute"
	    , ""
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
      ||
      null n
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

parseLs'		:: PathArrow AlbumTree Path -> String -> [String] -> [Cmd]
parseLs' pa ps		= parseWdCmd' ls ps
                          where
			  ls	= ( pa
				    />>>/
				    const (arrIO (putStrLn . mkAbsPath . joinPath))
				  )
				  `withDefaultRes` ()

parseGenHtml			:: String -> Bool -> [String] -> [Cmd]
parseGenHtml c rec []		= parseGenHtml c rec [".", ""]
parseGenHtml c rec [p]		= parseGenHtml c rec (p : [""])
parseGenHtml c rec (p:f:_)	= parseWdCmd' gen c [p]
                                  where
				  gen = getTreeAndProcess (withConfig (genHtml rec f))

parseCat 			:: String -> [String] -> [Cmd]
parseCat c			= parseWdCmd' cat c
                                  where
				  cat = (getTreeAndProcess (\ p -> constA p &&& getNode))
					/>>>/
					const (xpickleDocument xpAlbumEntry [ (a_indent, v_1)
									    , (a_no_xml_pi, v_1)
									    , (a_output_encoding, usAscii)
									    ] ""
					      )

parseDump			:: String -> [String] -> [Cmd]
parseDump c			= parseWdCmd' dump c
                                  where
				  dump = getTree
					 />>>/
					 const (xpickleDocument xpAlbumTree [ (a_indent, v_1)
									    , (a_no_xml_pi, v_1)
									    , (a_output_encoding, usAscii)
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

parseLoad		:: [String] -> [Cmd]
parseLoad		= parseWdCmd' ld "load"
                          where
			  ld = changeAlbums $
			       processTreeSelfAndDesc ( const $ this )

parseStore		:: String -> String -> [String] -> [Cmd]
parseStore f c		= parseWdCmd' (withConfig (storeAll f)) c

parseStoreAlbums	:: String -> [String] -> [Cmd]
parseStoreAlbums c	= parseWdCmd' storeAllChangedAlbums c

parseStorePics		:: String -> [String] -> [Cmd]
parseStorePics c	= parseWdCmd' storeAllChangedEntries c

parseImport		:: String -> [String] -> [Cmd]
parseImport c		= parseWdCmd' imp c
                          where
			  imp = changeAlbums $
				processTree (withConfig importPics)

parseTest		:: [String] -> [Cmd]
parseTest		= parseWdCmd' test "xxx"
                          where
			  test = changeAlbums $
				 processTree (withConfig importPics)
			  {-
			  test = const (get theConfig >>> arrIOE  scanForNewImages >>> arrIO print)
			  -}
			  {-
			  test = changeAlbums $
				 processTreeSelfAndDesc ( const $ perform $
							  getNode >>> arr picId >>> arrIO print
							)
							-}
parseUpdate		:: Bool -> String -> [String] -> [Cmd]
parseUpdate rec c	= parseWdCmd' (changeAlbums update) c
                          where
			  update = ( if rec then processTreeSelfAndDesc else processTree )
				   ( withConfig updatePic
				     />>>/
				     withConfig updateExifAttrs
				   )

parseNewAttrKeys	:: [String] -> [Cmd]
parseNewAttrKeys	= parseWdCmd' ( changeAlbums $
				        processTreeSelfAndDesc (withConfig updateAttrKeys)
				      ) "newattrs"

parseModifiy		:: String -> ConfigArrow AlbumTree AlbumTree -> [String] -> [Cmd]
parseModifiy cn ca al	= parseWdCmd' ( changeAlbums $
				        processTree (withConfig ca)
				      ) cn (take 1 al)

parseAttr		:: String -> [String] -> [Cmd]
parseAttr c al		= parseWdCmd' ( changeAlbums $
					processTree (withConfig (updateAttr an (unwords avl)))
				      ) c (take 1 al)
                          where
			  (an : avl) = tail al

parseDeleteAttr		:: String -> [String] -> [Cmd]
parseDeleteAttr c al	= parseWdCmd' ( changeAlbums $
					processTreeSelfAndDesc (deleteAttr ap)
				      ) c (take 1 al)
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
