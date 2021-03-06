module System.FindGrepSed
where

import           Control.Arrow
import           Control.Monad

import qualified Data.ByteString                        as B
import qualified Data.ByteString.Char8                  as C

import           Data.Char                              ( isSpace, toLower )
import           Data.List                              hiding ( find )
import           Data.Maybe
import qualified Data.Map                               as M
import qualified Data.Set                               as S

import           Data.String.Unicode                    ( utf8ToUnicode
                                                        , unicodeToUtf8
                                                        )
import           Text.Regex.XMLSchema.Generic           ( match )
import           Text.XML.HXT.Parser.XhtmlEntities      ( xhtmlEntities )

import System.IO
import System.Directory
import System.Posix.Files

data FindExpr
    = FPred     (FilePath -> IO Bool)
    | Ext       String
    | Name      String
    | DirName   String
    | FileName  String
    | RE        String
    | FT
    | FF
    | IsFile
    | IsDir
    | HasCont   (String -> Bool)
    | AndExpr   [FindExpr]
    | OrExpr    [FindExpr]
    | NotExpr   FindExpr

fe2T    :: FindExpr -> FilePath -> IO Bool

fe2T (FPred p) f
    = p f

fe2T (Ext ext) f
    = return (ext `isSuffixOf` f)

fe2T (Name f1) f
    = return (f1 == basename f)

fe2T (FileName f1) f
    = fe2T (RE f1) (basename f)

fe2T (DirName f1) f
    = fe2T (RE f1) (dirname f)

fe2T (RE re) f
    = return . match re $ f

fe2T FT f
    = trueFct f

fe2T FF f
    = falseFct f

fe2T IsFile f
    = do
      s <- getSymbolicLinkStatus f      -- followSymLinks: getFileStatus
      return (isRegularFile s)

fe2T IsDir f
    = do
      s <- getSymbolicLinkStatus f      -- followSymLinks: getFileStatus
      return (isDirectory s)

fe2T (HasCont p) f
    = (fe2T IsFile) `andFct` contentFind p $ f

fe2T (AndExpr fl) f
    = (foldl andFct trueFct . map fe2T) fl f

fe2T (OrExpr fl) f
    = (foldl orFct falseFct . map fe2T) fl f

fe2T (NotExpr e) f
    = do
      r <- (fe2T e) f
      return (not r)

-- ------------------------------

trueFct         :: FilePath -> IO Bool
trueFct         = return . const True

falseFct        :: FilePath -> IO Bool
falseFct        = return . const False

andFct  :: (FilePath -> IO Bool) -> (FilePath -> IO Bool) -> (FilePath -> IO Bool)
andFct fct1 fct2 f
    = do
      r1 <- fct1 f
      if r1
         then fct2 f
         else return False

orFct   :: (FilePath -> IO Bool) -> (FilePath -> IO Bool) -> (FilePath -> IO Bool)
orFct fct1 fct2 f
    = do
      r1 <- fct1 f
      if r1
         then return True
         else fct2 f

-- ------------------------------

contentFind     :: (String -> Bool) -> FilePath -> IO Bool
contentFind p f
    = do
      -- hPutStrLn stderr ( "contentFind: " ++ f )
      h <- openFile f ReadMode
      b <- B.hGetContents h
      hClose h
      return . p . C.unpack $ b

-- ------------------------------

isBinary                :: String -> Bool
isBinary                = not . isLatin1

isAscii                 :: String -> Bool
isAscii                 = all isAsciiChar

isLatin1                :: String -> Bool
isLatin1                = all isLatin1Char

isUmlaut                :: String -> Bool
isUmlaut s              = not (isUtf8 s) && isLatin1 s && not (isAscii s)

isUtfUmlaut             :: String -> Bool
isUtfUmlaut             = utf8ToUnicode >>> fst >>> isUmlaut

isUtf                   :: String -> Bool
isUtf s                 = not (isAscii s) && isUtf8 s

isUtf8                  :: String -> Bool
isUtf8 []               = True
isUtf8 (c:cs)
    | isAsciiChar c     = isUtf8 cs
    | isUtf2Char c      = isUtf81 cs
    | isUtf3Char c      = isUtf82 cs
    | otherwise         = False

isUtf81                 :: String -> Bool
isUtf81 (c:cs)
    | isUtf1Char c      = isUtf8 cs
isUtf81 _               = False

isUtf82                 :: String -> Bool
isUtf82 (c:cs)
    | isUtf1Char c      = isUtf81 cs
isUtf82 _               = False

hasTrailingWS           :: String -> Bool
hasTrailingWS           = not . null . takeWhile isSpace . reverse

hasTabs                 :: String -> Bool
hasTabs                 = any (== '\t')

-- ------------------------------------------------------------

isAsciiChar     :: Char -> Bool
isAsciiChar  c  = (' ' <= c && c <= '~') || (c `elem` "\n\t\r")

isLatin1Char    :: Char -> Bool
isLatin1Char c  = isAsciiChar c || ( '\160' <= c && c <= '\255')

isUtf1Char      :: Char -> Bool
isUtf1Char c    = '\128' <= c && c < '\192'

isUtf2Char      :: Char -> Bool
isUtf2Char c    = '\192' <= c && c < '\224'

isUtf3Char      :: Char -> Bool
isUtf3Char c    = '\224' <= c && c < '\240'


-- ------------------------------

findAllEntries  :: FilePath -> IO [FilePath]
findAllEntries dir
    = do
      -- hPutStrLn stderr ( "dir: " ++ dir )
      dir' <- if null dir
              then getCurrentDirectory
              else return dir
      entries <- getDirectoryContents dir'
      entries' <- mapM findRec (map (joinFile dir) . sort . filter (`notElem` [".", ".."]) $ entries)
      return (concat entries')
    where
    findRec path
        = do
          isD <- doesDirectoryExist path
          if isD
             then do
                  subentries <- findAllEntries path
                  return (path : subentries)
             else return [path]

-- ------------------------------

find    :: FilePath -> FindExpr -> IO [FilePath]
find dir test
    = do
      entries <- findAllEntries dir
      filterM (fe2T test) entries

-- ------------------------------

findExec        :: FilePath -> FindExpr -> (FilePath -> IO ()) -> IO ()
findExec dir test action
    = do
      entries <- find dir test
      mapM_ action $ entries

-- ------------------------------

-- filename manipulation

joinFile        :: FilePath -> FilePath -> FilePath
joinFile "" f   = f
joinFile d ""   = d
joinFile d f    = d ++ "/" ++ f

basename        :: FilePath -> FilePath
basename        = reverse . takeWhile (/= '/') . reverse

dirname         :: FilePath -> FilePath
dirname         = reverse . drop 1 . dropWhile (/= '/') . reverse

extension       :: FilePath -> FilePath
extension
    = reverse . takeWhile (/= '.') . reverse
      . hasDot . basename
    where
    hasDot s
        | all (== '.') s = ""
        | all (/= '.') s = ""
        | head s == '.'  = hasDot (tail s)
        | otherwise      = s

remTopDir       :: FilePath -> FilePath
remTopDir       = drop 1 . dropWhile (/= '/')

remExtensions   :: [String] -> FilePath -> FilePath
remExtensions es f
    | extFound
        = reverse . drop (length fe + 1) . reverse $ f
    | otherwise
        = f
    where
    fe = extension f
    extFound = fe `elem` es

-- ------------------------------

substXhtmlUtf8Chars     :: String -> String
substXhtmlUtf8Chars str
    | null errs
        = substXhtmlChars res
    | otherwise                 -- decoding errors, do nothing
        = str
    where
    (res, errs) = utf8ToUnicode str

substXhtmlChars :: String -> String
substXhtmlChars
    = concatMap transChar
    where
    transChar   :: Char -> String
    transChar c
        | isAsciiChar c
            = [c]
        | otherwise
            = ("&" ++)
              . (++ ";")
              . fromMaybe (("#" ++) . show .fromEnum $ c)
              . M.lookup (fromEnum c)
              $ xhtmlEntityMap

type EntityMap = M.Map Int String

xhtmlEntityMap  :: EntityMap
xhtmlEntityMap
    = M.fromList
      . map (\(x,y) -> (y,x))
      $ xhtmlEntities


substUmlauts    :: String -> String
substUmlauts    = substUmlauts' umlautMapAscii

substUmlautsTex :: String -> String
substUmlautsTex = substUmlauts' umlautMapTex

substUmlauts'    :: [(Char,String)] -> String -> String
substUmlauts' umlautMap
    = concatMap transUmlaut
    where
    transUmlaut c
        | isAsciiChar c
            = [c]
        | otherwise
            =  fromMaybe [c]
               . lookup c
               $ umlautMap

umlautMapAscii  :: [(Char,String)]
umlautMapAscii
    = [ ('\196', "Ae")
      , ('\214', "Oe")
      , ('\220', "Ue")
      , ('\223', "ss")
      , ('\228', "ae")
      , ('\246', "oe")
      , ('\252', "ue")
      ]

umlautMapTex    :: [(Char,String)]
umlautMapTex
    = [ ('\196', "\"A")
      , ('\214', "\"O")
      , ('\220', "\"U")
      , ('\223', "\\3")
      , ('\228', "\"a")
      , ('\246', "\"o")
      , ('\252', "\"u")
      ]

substLatin1Haskell      :: String -> String
substLatin1Haskell
    = concatMap transHaskellChar
    where
    transHaskellChar c
        | isAsciiChar c = [c]
        | otherwise     = init. tail . show $ c

substLatin1Tcl  :: String -> String
substLatin1Tcl
    = concatMap transTclChar
    where
    transTclChar c
        | isAsciiChar c = [c]
        | otherwise     = "\\x" ++ hexDigits 2 (fromEnum c)

substLatin1Tex  :: String -> String
substLatin1Tex  = substUmlautsTex

substUtf8Tex    :: String -> String
substUtf8Tex    = utf8ToUnicode
                  >>>
                  fst
                  >>>
                  substUmlautsTex
                  >>>
                  unicodeToUtf8

hexDigits       :: Int -> Int -> String
hexDigits n
    = reverse . take n . (++ (replicate n '0')) . reverse . toHx
    where
    toHx        :: Int -> String
    toHx i
        | i < 16        = [ cv !! i ]
        | otherwise     = toHx (i `div` 16) ++ toHx (i `mod` 16)
        where
        cv = "0123456789ABCDEF"

removeTrailingWS        :: String -> String
removeTrailingWS
    = unlines . map (reverse . dropWhile isSpace . reverse) . lines

removeTabs              :: String -> String
removeTabs
    = lines >>> map (mapAccumL rmTab 0 >>> snd >>> concat) >>> unlines
    where
    rmTab               :: Int -> Char -> (Int, String)
    rmTab i '\t'        = (i', replicate (i' - i) ' ')
                          where
                          i' = ((i + 8) `div` 8) * 8
    rmTab i ch          = (i+1, [ch])

-- ------------------------------

-- actions on found files

-- print

printFiles      :: [FilePath] -> IO ()
printFiles
    = putStr . concat . map (++ "\n")

-- print extensions

printFileExtensions     :: [FilePath] -> IO ()
printFileExtensions
    = printFiles . extensions
    where
    extensions
        = nub . sort . filter (not . null) . map extension

removeFiles     :: [FilePath] -> IO ()
removeFiles     = mapM_ remove1File . reverse

remove1File     :: FilePath -> IO ()
remove1File f
    = do
      fx <- doesFileExist f
      if fx
         then do
              hPutStrLn stderr ("remove file " ++ show f)
              removeFile f
         else do
              dx <- doesDirectoryExist f
              when dx (removeDir f)
    where
    isEmptyDir d
        = do
          dc <- getDirectoryContents d
          return $ null . filter (`notElem` [".",".."]) $ dc

    removeDir d
        = do
          de <- isEmptyDir d
          when de ( do
                    hPutStrLn stderr ("remove directory " ++ show d)
                    removeDirectory d
                  )

-- ------------------------------

-- | rename file
--
-- done in 2 steps for preventing Windows name claches with lowercase/uppercase names

moveFile2       :: FilePath -> IO ()
moveFile2 path
    = do
      hPutStrLn stderr ("rename " ++ show path ++ " to " ++ show pathNew)
      renameFile path pathTmp   -- 2 steps for preventing dos name claches
      renameFile pathTmp pathNew
    where
    pathTmp = path ++ ".tmp"
    pathNew = dirname path `joinFile` (map toLower . basename $ path)

-- ------------------------------

-- | grep lines from file contents

contentGrep     :: (String -> Bool) -> FilePath -> IO ()
contentGrep p f
    = do
      h <- openFile f ReadMode
      b <- B.hGetContents h
      putStr $ grep (lines . C.unpack $ b)
      hClose h
    where
    grep
        = concatMap format . filter (p . snd) . zip [(1::Int)..]
        where
        format (n, k) = f ++ ":" ++ show n ++ ": " ++ k ++ "\n"

-- ------------------------------

-- edit the contents of a file and make backup file

contentEdit     :: (String -> String) -> FilePath -> IO ()
contentEdit ef f
    = do
      h <- openFile f ReadMode
      b <- B.hGetContents h
      let c  = C.unpack b
      let c' = ef c
      if c' == c
         then hClose h
         else do
              hPutStrLn stderr ( "contentEdit: " ++ f )
              bak <- openFile (f ++ "~") WriteMode        -- make backup file
              B.hPutStr bak b
              hClose bak
              hClose h
              h' <- openFile f WriteMode
              B.hPutStr h' (C.pack c')
              hClose h'

-- ------------------------------

-- | find and process files

processFiles    :: ([FilePath] -> IO ()) -> FindExpr -> FilePath -> IO ()
processFiles action expr dir
    = find dir expr >>= action

findFiles       :: FindExpr -> FilePath -> IO ()
findFiles       = processFiles printFiles

findExts        :: FindExpr -> FilePath -> IO ()
findExts
    = processFiles (printFiles . extensions)
    where
    extensions
        = nub . sort . filter (not . null) . map extension

remFiles        :: FindExpr -> FilePath -> IO ()
remFiles        = processFiles removeFiles

grepFiles       :: (String -> Bool) -> FindExpr -> FilePath -> IO ()
grepFiles prd   = processFiles (mapM_ (contentGrep prd))

sedFiles        :: (String -> String) -> FindExpr -> FilePath -> IO ()
sedFiles edit   = processFiles (mapM_ (contentEdit edit))

moveFiles       :: FindExpr -> FilePath -> IO ()
moveFiles       = processFiles (mapM_ moveFile2)

-- ------------------------------

boringFiles     :: FindExpr
boringFiles
    = OrExpr [ Ext "~"
             , Ext ".bak"
             , Ext ".old"
             , Ext ".out"
             , Ext ".tmp"
             , Ext ".aux"
             , DirName "cache"
             , DirName ".xvpics"
             , Name ".directory"
             , FileName "[.]#.*"
             ]

cvsFiles        :: FindExpr
cvsFiles
    = OrExpr [ DirName "CVS"
             , Name ".cvsignore"
             ]

badNames        :: FindExpr
badNames
    = AndExpr [ NotExpr boringFiles,
                RE ".*[^-+,._/a-zA-Z0-9].*"
              ]

-- ------------------------------

binaryFiles     :: FindExpr
binaryFiles
    = OrExpr [ Ext ".a"
             , Ext ".bz2"
             , Ext ".class"
             , Ext ".dep"
             , Ext ".gz"
             , Ext ".fig"
             , Ext ".hi"
             , Ext ".gif"
             , Ext ".gz"
             , Ext ".hi"
             , Ext ".jar"
             , Ext ".jpg"
             , Ext ".jpeg"
             , Ext ".mov"
             , Ext ".mp3"
             , Ext ".nef"
             , Ext ".o"
             , Ext ".pdf"
             , Ext ".pgm"
             , Ext ".png"
             , Ext ".ppm"
             , Ext ".ps"
             , Ext ".psd"
             , Ext ".rpm"
             , Ext ".rws"
             , Ext ".tar"
             , Ext ".tgz"
             , Ext ".tif"
             , Ext ".tiff"
             , Ext ".war"
             , Ext ".wav"
             , Ext ".xbm"
             , Ext ".xcf"
             , Ext ".zip"
             ]

binaryContents  :: FindExpr
binaryContents
    = AndExpr [ NotExpr textFiles
              , HasCont isBinary
              ]

textFiles       :: FindExpr
textFiles
    = OrExpr [ progFiles
             , xmlFiles
             , htmlFiles
             , texFiles
             , Ext ".txt"
             , Name "readme"
             , Name "README"
             , Name "INSTALL"
             , Name "LICENSE"
             ]

texFiles        :: FindExpr
texFiles
    = OrExpr [ Ext ".tex"
             , Ext ".sty"
             ]

xmlFiles        :: FindExpr
xmlFiles
    = OrExpr [ Ext ".dtd"
             , Ext ".ent"
             , Ext ".rng"       -- Relax NG
             , Ext ".xml"
             , Ext ".xhtml"
             , Ext ".xsl"       -- XSLT
             ]

hxmlFiles       :: FindExpr
hxmlFiles
    = OrExpr [ xmlFiles
             , Ext ".html"
             , Ext ".htm"
             ]

htmlFiles       :: FindExpr
htmlFiles
    = OrExpr [ Ext ".htm"
             , Ext ".html"
             , Ext ".style"
             , Name ".htaccess"
             , RE ".*/automata/.*[.]tab"        -- CB first and follow tables
             ]

makeFiles       :: FindExpr
makeFiles
    = OrExpr [ Name "Makefile"
             , Name "makefile"
             ]

progFiles'      :: FindExpr
progFiles'
    = AndExpr [ NotExpr makeFiles
              , progFiles
              ]

progFiles       :: FindExpr
progFiles
    = OrExpr [ makeFiles
             , Ext ".ass"       -- ppl assembler
             , Ext ".c"
             , Ext ".cc"
             , Ext ".check"     -- ppl parser
             , Ext ".css"
             , Ext ".cup"       -- CUP input
             , Ext ".diffcode"  -- ppl ass diff
             , Ext ".dot"       -- dot graph input
             , Ext ".exp"
             , Ext ".gencode"   -- ppl code gen
             , Ext ".h"
             , Ext ".hs"
             , Ext ".java"
             , Ext ".js"
             , Ext ".lex"
             , Ext ".lhs"
             , Ext ".optcode"   -- ppl code gen
             , Ext ".parse"     -- ppl parser output
             , Ext ".pl"
             , Ext ".ppl"       -- ppl source
             , Ext ".scan"      -- ppl lexer output
             , Ext ".sh"
             , tclFiles
             , Ext ".trc"       -- ppl trace output
             , Ext ".x"         -- lex input
             , Ext ".y"
             ]

haskellFiles    :: FindExpr
haskellFiles
    = Ext ".hs"

tclFiles        :: FindExpr
tclFiles
    = OrExpr [ Ext ".cgi"
             , Ext ".tcl"
             ]

unknownFiles    :: FindExpr
unknownFiles
    = AndExpr [ NotExpr $
                OrExpr [ binaryFiles
                       , textFiles
                       , cvsFiles
                       , boringFiles
                       ]
              , IsFile
              ]

-- ------------------------------

executableFiles :: FindExpr
executableFiles
    = AndExpr [ IsFile
              , NotExpr boringFiles
              , FPred (\ f -> fileAccess f False False True)
              ]

fileExtensions  :: FindExpr
fileExtensions
    = AndExpr [ IsFile
              , NotExpr boringFiles
              , NotExpr cvsFiles
              ]

htmlLatin1Files :: FindExpr
htmlLatin1Files
    = AndExpr [ hxmlFiles
              , HasCont isUmlaut
              ]

htmlUtf8Files   :: FindExpr
htmlUtf8Files
    = AndExpr [ hxmlFiles
              , HasCont isUtf
              ]

asciiFiles      :: FindExpr
asciiFiles
    = AndExpr [ textFiles
              , NotExpr hxmlFiles
              , HasCont isAscii
              ]

noneAsciiProgFiles      :: FindExpr
noneAsciiProgFiles
    = AndExpr [ progFiles
              , HasCont isUmlaut
              ]

noneAsciiHaskellFiles   :: FindExpr
noneAsciiHaskellFiles
    = AndExpr [ haskellFiles
              , HasCont isUmlaut
              ]

tclLatin1Files  :: FindExpr
tclLatin1Files
    = AndExpr [ tclFiles
              , HasCont isUmlaut
              ]

texLatin1Files  :: FindExpr
texLatin1Files
    = AndExpr [ texFiles
              , HasCont isUmlaut
              ]

texUtf8Files  :: FindExpr
texUtf8Files
    = AndExpr [ texFiles
              , HasCont isUtf
              , HasCont isUtfUmlaut
              ]

trailingBlankFiles      :: FindExpr
trailingBlankFiles
    = AndExpr [ textFiles
              , HasCont (any hasTrailingWS . lines)
              ]

-- ------------------------------

uppercaseImgFiles       :: FindExpr
uppercaseImgFiles
    = OrExpr [ AndExpr [ FileName "([_A-Z]+)[0-9]+(-[0-9]+)?[.](XMP|xmp|NEF|nef|JPG|jpg|MOV|mov|MP3|mp3|TIF|tif|RW2|rw2|WAV|wav|((NEF|nef)[.](DOP|dop|RWS|rws)))"
                       , FileName ".*[A-Z].*"           -- at least 1 uppercase char
                       ]
             , FileName "[0-9]+-[0-9]+[.](WAV|MP3)"     -- zoom mic
             ]

nonAsciiFilePath        :: FindExpr
nonAsciiFilePath
    = FPred $ return . (any (\c -> c < '\x20' || c > '\x7F'))

-- ------------------------------

type ImgSet = S.Set FilePath

processUnusedAlbumFiles :: ([FilePath] -> IO ()) -> FilePath -> IO ()
processUnusedAlbumFiles action dir
    = do
      when (not . null $ dir) (setCurrentDirectory dir)
      images <- getImageList
      unused <- find "" (filterEx images)
      action unused
    where
    subdirs = [ "org"
              , "1920x1200"
              , "html-1920x1200"
              , "1600x1200"
              , "html-1600x1200"
              , "1600x1200-css"
              , "1400x1050"
              , "html-1400x1050"
              , "160x160"
              , "160x120"
              ]
    fileExt = [ ".jpg"
              , ".html"
              , ".html"
              ]
    filterEx is = AndExpr
                  [ RE
                    . (++ ")/.*") . ("(" ++)
                    . foldl1 (\ x y -> x ++ "|" ++ y)
                    $ subdirs
                  , NotExpr . FPred $ (imgInUse is)
                  ]

    imgInUse    :: ImgSet -> FilePath -> IO Bool
    imgInUse is fp
        = return $ fp' `S.member` is
        where
        fp' = remExtensions fileExt . remTopDir $ fp

    getImageList
        = do
          h <- openFile "picturelist.txt" ReadMode
          c <- hGetContents h
          return $! (S.fromList . filter (not . null) . lines $ c)

-- ------------------------------
