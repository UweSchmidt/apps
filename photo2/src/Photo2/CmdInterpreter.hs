module Photo2.CmdInterpreter
where

import           Control.DeepSeq

import           Data.Atom
import           Data.Char
import           Data.List
import qualified Data.Map                    as M
import           Data.Maybe

import           Photo2.ArchiveTypes
import           Photo2.Arrow
import           Photo2.FilePath
import           Photo2.Html
import           Photo2.ImportDialog

import           System.Process              (system)

import           Text.Regex.XMLSchema.String (match, tokenize')
import           Text.XML.HXT.Core

-- ------------------------------------------------------------

tokenizeCmdLine :: String -> Either String [String]
tokenizeCmdLine = checkTokens . tokenize' tokens

checkTokens :: [Either String String] -> Either String [String]
checkTokens ts
    | tokensOk  = Right . map unescape . filter ( not . all isSpace) . concatMap (either (const []) (:[])) $ ts
    | otherwise = Left . concatMap (either id id) $ ts
    where
    tokensOk = all (either (const False) (const True)) $ ts

    unescape s@('\"' : _)       = filter (/= '\\') . init . tail $ s
    unescape s                  = s

tokens  :: String
tokens  = intercalate "|" . map par $ toks
          where
          toks = [ "[^ \\n\\t\\r;" ++ q ++ bsbs ++ "]+"
                 , q ++ "([^" ++ bsbs ++ q ++ "]|(" ++ bsbs ++ ".))*" ++ q
                 , ";"
                 , "[ \\n\\t\\r]+"
                 ]
          par s = "(" ++ s ++ ")"
          bsbs  = bs ++ bs
          q     = "\""
          bs    = "\\"

parseCmdLine    :: Either String [String] -> [Cmd]
parseCmdLine (Left line)        = illegalCmd "" [line]
parseCmdLine (Right ts)         = concatMap parseCmd' . splitCmds $ ts
                                  where
                                  parseCmd' []          = []
                                  parseCmd' (c:args)    = parseCmd c args

                                  splitCmds []          = [[]]
                                  splitCmds (";" : rs)  = [] : splitCmds rs
                                  splitCmds (c : rs)    = let (cs:rss) = splitCmds rs in (c:cs):rss

-- ------------------------------------------------------------

type Cmd = AppState -> IO AppState

cmdLoop :: (String -> IO String) -> Cmd
cmdLoop readCmdLine state
    = do
      line <- readCmdLine prompt
      let cmds = parseCmdLine . tokenizeCmdLine $ line
      if null cmds
         then return state
         else do
              newState <- runCmds cmds state
              rnf newState `seq` cmdLoop readCmdLine newState
    where
    prompt = ("photo2@" ++) . (++ "> ") . mkAbsPath . joinPath . cwd $ state
    runCmds [] s0
        = return s0
    runCmds (c:cs) s0
        = do
          s1 <- c s0
          runCmds cs s1

mkCmd                   :: CmdArrow a b -> [Cmd]
mkCmd                   = return . runCmd

mkCmd'                  :: (Monad m) => CmdArrow a b -> ([b] -> IO ()) -> m Cmd
mkCmd' io               = return . runCmd' io

liftCmd                 :: Monad m => IO a -> m Cmd
liftCmd c               = return $
                          \ s -> ( do
                                   _ <- c
                                   return s )

-- ------------------------------------------------------------

align   :: [String] -> [String]
align xs
    = map ( take maxLen . (++ replicate maxLen ' ')) $ xs
    where
    maxLen = maximum . (0:) . map length $ xs

fmtTable        :: String -> [(String, String)] -> [String]
fmtTable del ts
    = zipWith (\ x y -> x ++ del ++ y)
              (align . map fst $ ts)
              (map snd $ ts)

-- ------------------------------------------------------------

parseCmd                                                :: String -> [String] -> [Cmd]
parseCmd c@"open" []                                    = parseCmd c ["archive2.xml"]
parseCmd   "open" [archive]                             = mkCmd ( loadArchiveAndConfig archive
                                                                  >>>
                                                                  rootWd
                                                                  >>>
                                                                  withCwd loadAlbums
                                                                )
parseCmd "config" []                                    = mkCmd ( get theConfig
                                                                  >>>
                                                                  xpickleDocument xpConfig [ withIndent yes
                                                                                           , withXmlPi  no
                                                                                           , withOutputEncoding usAscii
                                                                                           ] ""
                                                                )
parseCmd "options" []                                  = parseCmd "options" [".*"]
parseCmd "options" [pat]                               = mkCmd ( get theConfigAttrs
                                                                  >>>
                                                                  arr dumpOptions
                                                                  >>>
                                                                  putRes
                                                                )
                                                          where
                                                          dumpOptions = unlines . fmtTable " = "
                                                                        .
                                                                        filter (match pat . fst)
                                                                        .
                                                                        map (first show) . M.toList

parseCmd "get" [n]                                      = mkCmd ( get theConfigAttrs
                                                                  >>>
                                                                  arr ( M.lookup (newAtom n)
                                                                        >>>
                                                                        fromMaybe ""
                                                                      )
                                                                  >>>
                                                                  putRes
                                                                )
parseCmd c@"set" [n]                                    = parseCmd c [n,v_1]
parseCmd   "set" [n,v]                                  = mkCmd ( changeComp theConfigAttrs (M.insert (newAtom n) v) )
parseCmd c@"set" (n:vl@(_:_))                           = parseCmd c (n : unwords vl : [])

parseCmd "unset" [n]                                    = mkCmd ( changeComp theConfigAttrs (M.delete (newAtom n)  ) )
parseCmd "defpicattr" [n,v]                             = mkCmd ( changeComp theConfigPicAttrs (addEntry v n) )
parseCmd "pwd" []                                       = mkCmd ( get theWd
                                                                  >>>
                                                                  arr ((rootPath </>) . joinPath) >>> putRes
                                                                )
parseCmd c@"ls"            args                         = parseLs' (getTreeAndProcessChildren constA) c args
parseCmd c@"ls-all"        args                         = parseLs' (getTreeAndProcessDesc constA)     c args
parseCmd c@"ls-rec"        args                         = parseLs' (getTreeAndProcessDescC constA)    c args
parseCmd c@"ls-mod"        args                         = parseLs' (getTreeAndProcessSelfAndDesc $
                                                                    \ p -> entryEdited `guards` constA p
                                                                   )                                  c args
parseCmd c@"list"          args                         = parseLs'' (getTreeAndProcessChildren constA) c args

parseCmd c@"cat"           args                         = parseCat                                    c args
parseCmd c@"isalbum"       args                         = parseIsAlbum                                c args
parseCmd c@"dump"          args                         = parseDump                                   c args

parseCmd "relatives" args       = parseRelatives args
parseCmd "load"      args       = parseLoad      args

parseCmd c@"store"         args                         = parseStore ""                               c args
parseCmd c@"store-picture" args                         = parseStore "picture"                        c args
parseCmd c@"store-album"   args                         = parseStore "album"                          c args
parseCmd   "store-config" []                            = mkCmd storeConfig

parseCmd   "close"        []                            = mkCmd ( withRootDir (withConfig (storeAll ""))
                                                                  >>>
                                                                  storeConfig
                                                                  >>>
                                                                  storeArchive
                                                                )
                                                          ++
                                                          parseCd []


parseCmd c@"update"        args                         = parseUpdate False                           c args
parseCmd c@"update-all"    args                         = parseUpdate True                            c args

parseCmd c@"newattrs"      args                         = parseNewAttrKeys                            c args
parseCmd c@"newformat"     args                         = parseNewFormat False                        c args
parseCmd c@"newformat-all" args                         = parseNewFormat True                         c args

parseCmd c@"attr"          args | length args >= 2      = parseAttr                                   c args
parseCmd c@"deleteattr"    args | length args == 2      = parseDeleteAttr                             c args
parseCmd c@"sortalbum"     args | length args <= 1      = parseSort                                   c args
parseCmd c@"sortpictures"  args | length args >= 1      = parseSortPictures                           c args
parseCmd c@"import"        args | length args <= 1      = parseImport                                 c args
parseCmd c@"newalbum"      args | length args == 2      = parseModify c (newAlbum    (concat . drop 1 $ args)) args
parseCmd c@"setalbumpic"   args | length args == 2      = parseModify c (setAlbumPic (concat . drop 1 $ args)) args
parseCmd c@"rename"        args | length args == 2      = parseModify c (renamePic   (concat . drop 1 $ args)) args
parseCmd c@"removepicture" args | length args == 2      = parseModify c (removePicture (concat . drop 1 $ args)) args
parseCmd c@"makealbum"     args | length args <= 1      = parseModify c (makeAlbum)   args
parseCmd c@"makepicture"   args | length args <= 1      = parseModify c (makePicture) args

parseCmd c@"copypicture"   [a1, a2]
                                | "/" `isPrefixOf` a2   = parseCopyPic c a1 a2

parseCmd c@"rename-cont"   args | length args <= 1      = parseModify c renameContent args

parseCmd c@"dirty"         args | length args <= 1      = parseCleanup c False False args
parseCmd c@"dirty-all"     args | length args <= 1      = parseCleanup c False True  args
parseCmd c@"cleanup"       args | length args <= 1      = parseCleanup c True  False args
parseCmd c@"cleanup-all"   args | length args <= 1      = parseCleanup c True  True  args

parseCmd c@"html"          args                         = parseGenHtml c False args
parseCmd c@"html-all"      args                         = parseGenHtml c True  args

parseCmd "find"      args
    | length args `elem` [1..3] = parseFind             args

parseCmd "cd"        args       = parseCd               args

parseCmd "xxx" args             = parseTest             args

parseCmd "?" []
    = outputCmd usage
    where
    usage = unlines $
            [ "commands available from the promt:"
            , ""
            , "  ?                          help (this text)"
            , ""
            , "  open [archive]             load a photo archive, configuration and root album, default is \"archive2.xml\""
            , "  load [path]                load all subalbums and pictures"
            , "  store [path]...            write all albums and unload subalbums, format is given by option \"store-format\""
            , "  store-album   [path]...    write all pictures and albums in album format"
            , "  store-picture [path]...    write all pictures and albums in picture format"
            , "  store-config               write the config data"
            , "  close                      write the whole data, albums, config and archive"
            , "  exit,q                     exit photo2"
            , ""
            , "  options                    list options"
            , "  get <opt>                  get the value of an option"
            , "  set <opt> [val]...         set or overwrite an option, default value is \"1\""
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
            , "  newalbum    path newid     create a new album within path"
            , "  setalbumpic path id        set the album picture from the list of pictures within the album"
            , "  sortalbum  [path]          sort pictures by date"
            , "  import     [path]          star import dialog for adding new pictures into album"
            , "  update     [path]...       copy original and update copies, if original has changed"
            , "  update-all [path]...       recursively copy originals and update copies, if originals have changed"
            , ""
            , "  html     [p] [f]...        generate single HTML page for list of formats,"
            , "                             default album is current album, default format is given by option \"layout\""
            , "  html-all [p] [f]...        generate all HTML pages for list of formats,"
            , "                             default album is current album, default format is given by option \"layout\""
            , ""
            , "  attr path n [vl]           set attribute value for picture/album or unset attr, if vl is missing"
            , "  deleteattr path pattern    delete all attributes matching the attribute name pattern"
            , "  newattrs  [path]           change attribute keys to new format"
            , "  newformat     [path]       change entry into new album format"
            , "  newformat-all [path]       change all entries into new album format"
            , ""
            -- , "  rename-cont [path]         rename all pictures in an album"
            , "  rename path newid          rename picture"
            , "  defpicattr a val           define a picture attribute"
            , ""
            , "  dirty       [path]         list unused files"
            , "  dirty-all   [path]         list unused files recursively"
            , "  cleanup     [path]         cleanup image dirs"
            , "  cleanup-all [path]         cleanup image dirs recursively"
            , ""
            , "  !<shell command>           shell command"
            , "  version                    print photo2 version"
            ]

parseCmd "version" []
    = outputCmd $ "Photo2 version 0.2.7.2 from 2014-09-23"
      -- for photoEdit please change the version in config/photoEdit/MainWindow.glade

parseCmd "exit" _       = fail ""
parseCmd "q" _          = fail ""

parseCmd ('!':cmd) args = liftCmd $
                          do
                          _ <- system (unwords $ cmd : args)
                          return ()

parseCmd c args         = illegalCmd c args

-- ------------------------------------------------------------

outputCmd       :: (Monad m) => String -> m Cmd
outputCmd msg
    = return out
      where
      out aState
          = do
            load selWriteRes aState $ msg
            return aState

illegalCmd      :: (Monad m) => String -> [String] -> m Cmd
illegalCmd c args
    = return illegal
      where
      illegal aState
          = do
            load selWriteLog aState $
                    "unknown command or wrong arguments: "
                    ++ unwords (c : args) ++ "\n"
                    ++ "try ? for help"
            return aState

-- ------------------------------------------------------------
--
-- execute a command with one argument, a path for addressing one node
-- if the path is empty, the currend working dir is taken,
-- if it's a relative path, it's adressed via current working dir

parseWdCmd      :: PathArrow a b -> String -> [String] -> [Cmd]
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
    cFailed     = errMsg ("command failed: " ++ unwords (name : ps))
                  >>>
                  none
    (n : ps')   = ps
    p@(_:p')    = splitPath n


parseWdCmd'     :: PathArrow AlbumTree b -> String -> [String] -> [Cmd]
parseWdCmd' pa  = parseWdCmd (loadAndCheckAlbum />>>/ pa)

parseWdCmds'    :: PathArrow AlbumTree b -> String -> [String] -> [Cmd]
parseWdCmds' pa n []
                = parseWdCmd (loadAndCheckAlbum />>>/ pa) n []
parseWdCmds' pa n ps
                = concatMap (\ p -> parseWdCmd' pa n [p]) ps

-- ------------------------------------------------------------

findEntries     :: PathArrow a AlbumTree ->
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

showXmlVal      :: PU a -> PathArrow a ()
showXmlVal pk
    = const $
      ( xpickleVal pk
        >>>
        writeDocumentToString [ withIndent yes
                              , withXmlPi  no
                              , withOutputEncoding unicodeString -- utf8 -- usAscii
                              ]
        >>>
        putRes
      )

-- ------------------------------------------------------------

parseLs'                        :: PathArrow AlbumTree Path -> String -> [String] -> [Cmd]
parseLs' pa ps                  = parseWdCmd' ls ps
                                  where
                                  ls = ( pa
                                         />>>/
                                         const (arr (mkAbsPath . joinPath) >>> putRes)
                                       )
                                       `withDefaultRes` ()

parseLs''                       :: PathArrow AlbumTree Path -> String -> [String] -> [Cmd]
parseLs'' pa ps                 = parseWdCmd' ls ps
                                  where
                                  ls p = listA ( pa p
                                                 >>>
                                                 arr (mkAbsPath . joinPath)
                                               )
                                         >>>
                                         arr unwords
                                         >>>
                                         putRes

parseCleanup                    :: String -> Bool -> Bool -> [String] -> [Cmd]
parseCleanup c ex rec []        = parseCleanup c ex rec ["."]
parseCleanup c ex rec (p:_)     = parseWdCmd' gen c [p]
                                  where
                                  gen = getTreeAndProcess (withConfig (cleanupImgDirs ex rec))

parseGenHtml                    :: String -> Bool -> [String] -> [Cmd]
parseGenHtml c rec []           = parseGenHtml c rec [".", ""]
parseGenHtml c rec [p]          = parseGenHtml c rec (p : [""])
parseGenHtml c rec [p,f]        = parseWdCmd' gen c [p]
                                  where
                                  gen = getTreeAndProcess (withConfig (genHtml rec f))
parseGenHtml c rec (p:fl)       = concatMap (\ f -> parseGenHtml c rec [p,f]) fl

parseIsAlbum                    :: String -> [String] -> [Cmd]
parseIsAlbum c                  = parseWdCmd' isAlbum' c
                                  where
                                  isAlbum' p = getTree p
                                              >>>
                                              getNode
                                              >>>
                                              arr (show . isAl)
                                              >>>
                                              putRes

parseCat                        :: String -> [String] -> [Cmd]
parseCat c                      = parseWdCmd' cat c
                                  where
                                  cat = (getTreeAndProcess (\ p -> constA p &&& getNode))
                                        />>>/
                                        showXmlVal xpAlbumEntry

parseDump                       :: String -> [String] -> [Cmd]
parseDump c                     = parseWdCmd' dump c
                                  where
                                  dump = getTree
                                         />>>/
                                         showXmlVal xpAlbumTree

parseRelatives          :: [String] -> [Cmd]
parseRelatives          = parseWdCmd' relatives "relatives"
                          where
                          relatives
                              = getRelatives
                                />>>/
                                (\ p -> ( arr ( \ (parent, prev, next) ->
                                                ( "this     = " ++ fp p      ++ "\n" ++
                                                  "parent   = " ++ fp parent ++ "\n" ++
                                                  "previous = " ++ fp prev   ++ "\n" ++
                                                  "next     = " ++ fp next
                                                )
                                              )
                                          >>>
                                          putRes
                                        )
                                )
                          fp [] = ""
                          fp p  = mkAbsPath . joinPath $ p

parseLoad               :: [String] -> [Cmd]
parseLoad               = parseWdCmd' ld "load"
                          where
                          ld = changeAlbums $
                               processTreeSelfAndDesc ( const $ this )

parseStore              :: String -> String -> [String] -> [Cmd]
parseStore f c          = parseWdCmds' (withConfig (storeAll f)) c

parseStoreAlbums        :: String -> [String] -> [Cmd]
parseStoreAlbums c      = parseWdCmd' storeAllChangedAlbums c

parseStorePics          :: String -> [String] -> [Cmd]
parseStorePics c        = parseWdCmd' storeAllChangedEntries c

parseSort               :: String -> [String] -> [Cmd]
parseSort c             = parseWdCmd' srt c
                          where
                          srt = changeAlbums $
                                processTree (withConfig sortPics)

parseSortPictures       :: String -> [String] -> [Cmd]
parseSortPictures c al  = parseWdCmd' ( changeAlbums $
                                        processTree (sortPictures $ drop 1 al)
                                      ) c (take 1 al)

parseImport             :: String -> [String] -> [Cmd]
parseImport c           = parseWdCmd' imp c
                          where
                          imp = changeAlbums $
                                processTree (withConfig importPics)

parseUpdate             :: Bool -> String -> [String] -> [Cmd]
parseUpdate rec c       = parseWdCmds' (changeAlbums update) c
                          where
                          update = ( if rec then processTreeSelfAndDesc else processTree )
                                   ( withConfig updatePic
                                     />>>/
                                     withConfig updateExifAttrs
                                   )

parseNewFormat          :: Bool -> String -> [String] -> [Cmd]
parseNewFormat rec c    = parseWdCmd' ( changeAlbums $
                                        ( if rec
                                          then processTreeSelfAndDesc'
                                          else processTree
                                        ) newFormat
                                      ) c
                          where
                          newFormat = loadProcessStore
                                      ( withConfig updateAttrKeys
                                        />>>/
                                        deleteAttr "unknown:.*"
                                        />>>/
                                        withConfig updateExifAttrs
                                      )

parseNewAttrKeys        :: String -> [String] -> [Cmd]
parseNewAttrKeys c      = parseWdCmd' ( changeAlbums $
                                        processTreeSelfAndDesc (withConfig updateAttrKeys)
                                      ) c

parseModify             :: String -> ConfigArrow AlbumTree AlbumTree -> [String] -> [Cmd]
parseModify c ca al     = parseWdCmd' ( changeAlbums $
                                        processTree (withConfig ca)
                                      ) c (take 1 al)

parseCopyPic            :: String -> String -> String -> [Cmd]
parseCopyPic c a1 a2
                        = parseWdCmd' ( changeAlbums $
                                        withConfig (copyPicture p2)
                                      ) c [a1]
                        where
                        p2 = drop 1 . splitPath $ a2

parseAttr               :: String -> [String] -> [Cmd]
parseAttr c al          = parseWdCmd' ( changeAlbums $
                                        processTree (withConfig (updateAttr an (unwords avl)))
                                      ) c (take 1 al)
                          where
                          (an : avl) = tail al

parseDeleteAttr         :: String -> [String] -> [Cmd]
parseDeleteAttr c al    = parseWdCmd' ( changeAlbums $
                                        processTreeSelfAndDesc (deleteAttr ap)
                                      ) c (take 1 al)
                          where
                          (ap : _) = tail al

-- TODO refactor

parseFind               :: [String] -> [Cmd]
parseFind al            = parseWdCmd fe "find" (take 1 al)
                          where
                          rek   = concat . take 1 . drop 1 $ al
                          rev   = concat          . drop 2 $ al
                          fe    = findEntries loadAlbums (getAllWithAttr rek rev) puts
                          puts (p, k, v)
                              = putStrLn ( intercalate ": "                     -- remove putStrLn
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
        | null p        -- above the top of the roof
            = none
        | otherwise
            = loadAndCheckAlbum p
              >>>
              ( constA p >>> set theWd )
        where
        p = normalPath p0

-- ------------------------------------------------------------

parseTest               :: [String] -> [Cmd]
parseTest ps            = parseWdCmd test "xxx" ps
                          where
                          test p = findAlbumPath p
                                   >>>
                                   arrIO print
                          {-
                          test = changeAlbums $
                                 processTree (withConfig importPics)
                          -}
                          {-
                          test = const (get theConfig >>> arrIOE  scanForNewImages >>> arrIO print)
                          -}
                          {-
                          test = changeAlbums $
                                 processTreeSelfAndDesc ( const $ perform $
                                                          getNode >>> arr picId >>> arrIO print
                                                        )
                                                        -}
-- ------------------------------------------------------------
