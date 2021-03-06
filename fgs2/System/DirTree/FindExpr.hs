{- | module for evaluating find expressions -}

module System.DirTree.FindExpr
    ( module System.DirTree.FindExpr
    , matchRE
    , sedRE
    )
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.Char                         ( isSpace )
import Data.List                         ( mapAccumL )
import Data.Maybe

import Text.Regex.XMLSchema.Generic

import Text.XML.HXT.Parser.XhtmlEntities ( xhtmlEntities )

import System.DirTree.Types
import System.DirTree.FilePath
import System.DirTree.FileSystem

import qualified Data.Map                as M

-- ------------------------------

findExpr2FindPred :: FindExpr -> FindPred

findExpr2FindPred (MatchRE re) f
    = return $ matchRE re f

findExpr2FindPred (MatchExtRE re) f
    = return $ matchRE (mkSeqs [mkRep 1 mkDot, mkSym1 '.', re]) f

findExpr2FindPred (MatchPathRE re) f
    = pathName f >>= return . matchRE re

findExpr2FindPred FTrue f
    = truePred f

findExpr2FindPred FFalse f
    = falsePred f

findExpr2FindPred (HasType t) f
    = typeTest t f

findExpr2FindPred (HasFeature s p) f
    = do trc $ unwords ["hasFeature:", s ++ ":", show f]
         p f

findExpr2FindPred (HasCont s p) f
    = do trc $ unwords ["hasCont:",    s ++ ":", show f]
         isFile `andPred` (readFileContentsAsString >=> p) $ f

findExpr2FindPred (AndExpr e1 e2) f
    = andPred (findExpr2FindPred e1) (findExpr2FindPred e2) f

findExpr2FindPred (OrExpr e1 e2) f
    = orPred (findExpr2FindPred e1) (findExpr2FindPred e2) f

findExpr2FindPred (NotExpr e) f
    = not <$> findExpr2FindPred e f

typeTest :: EntryType -> FindPred
typeTest IsFile      = isFile
typeTest IsDir       = isDir
typeTest IsSymLink   = isSymLink
typeTest IsCharDev   = isCharDev
typeTest IsBlockDev  = isBlockDev
typeTest IsSocket    = isSock
typeTest IsNamedPipe = isPipe

-- ------------------------------

truePred :: FindPred
truePred = return . const True

falsePred :: FindPred
falsePred  = return . const False

andPred  :: FindPred -> FindPred -> FindPred
andPred fct1 fct2 f
    = do
      r1 <- fct1 f
      if r1
         then fct2 f
         else return False

orPred   :: FindPred -> FindPred -> FindPred
orPred fct1 fct2 f
    = do
      r1 <- fct1 f
      if r1
         then return True
         else fct2 f

-- ------------------------------
--
-- should be part of hxt-regex-xmlschema

checkRegex :: String -> Maybe Regex
checkRegex
    = (\ re -> if isZero re then Nothing else Just re) . parseRegex

checkContextRegex :: String -> Maybe Regex
checkContextRegex
    = (\ re -> if isZero re then Nothing else Just re) . parseContextRegex parseRegex

checkPathRegex :: String -> Maybe [Regex]
checkPathRegex s
    | null p
        = (:[]) <$> checkRegex d
    | otherwise
        = do re1 <- checkRegex d
             res <- checkPathRegex $ tail p
             return $ re1 : res
    where
      (d, p) = span (/= '/') s

-- If b is set, a string list shorter than the regex list is true,
-- so every path prefix matches the regex list.
-- This matching function is used to control the descending into the dir tree.
-- If b is set to false, the string list (the path) must be longer than the regex list,
-- this match is used for selecting the entries to be processed

matchSeqRE :: Bool -> [Regex] -> [String] -> Bool
matchSeqRE b _            [] = b
matchSeqRE _ []            _ = True
matchSeqRE b (r1:rs) (n1:ns) = matchRE r1 n1 && matchSeqRE b rs ns

matchPath :: Bool -> [Regex] -> FilePath -> Bool
matchPath b re = matchSeqRE b re . splitPath

-- ------------------------------

isBoringFile :: FindPred
isBoringFile = findExpr2FindPred boringFiles
    where
      boringFiles
          = orExprSeq [ matchExtRE "bak|old|out|tmp|aux|log|[~]"
                      , matchNameRE "\\..*"
                      , matchNameRE "[.]#.*"
                      ]

isUpperCaseImgFile :: FindPred
isUpperCaseImgFile = findExpr2FindPred upperCaseImgFiles
    where
    upperCaseImgFiles
        = orExprSeq
          [ andExprSeq
            [ matchNameRE $
              concat [ "([_A-Z]+)[0-9]+(-[0-9]+)?[.]"
                     , "(XMP|xmp|NEF|nef|JPG|jpg|MOV|mov"
                     , "|MP3|mp3|TIF|tif|RW2|rw2|WAV|wav"
                     , "|((NEF|nef)[.](DOP|dop|RWS|rws)))"
                     ]
            , matchNameRE ".*[A-Z].*"                   -- at least 1 uppercase char
            ]
          , matchNameRE "[0-9]+-[0-9]+[.](WAV|MP3)"     -- zoom mic
          ]


-- test whether a picture or html file has a corresponding .xml description file
--
-- used for album maintenance

isAlbumFile :: FindPred
isAlbumFile f
    = do p0 <- pathName f
         let p1 = remDir1 . normPath $ p0
         if null p1
            then return False
            else do l <- asks theLevel
                    -- d <- pwd
                    -- trc $ "isAlbumFile: " ++ show (d ++ "/" ++ albumFile p1 l)
                    isFile (albumFile p1 l)
    where
      albumFile p l
          = joinPath $ replicate l ".." ++ ["album2", p']
          where
            p' = (`joinExt` "xml") . remExt $ p

{-
isInAlbum :: FindPred
isInAlbum f
    = do ap <- asks ( joinFileName ".." . (`joinExt` "xml") . getFileName . theCwd)
         pp <- remDir1 <$> pathName f
         trc $ "isInAlbum: looking for " ++ show pp ++ " in album file: " ++ show ap
         ex <- isFile pp
         if not ex
            then return False
            else do
                    return undefined

-- -}

-- ------------------------------

imageFiles     :: FindExpr
imageFiles
    = andExprSeq [ HasType IsFile
                 , matchExtRE "bmp|gif|jpg|jpeg|mov|nef|pbm|pgm|png|ppm|psd|rw2|tif|tiff|xmb|xcf"
                 ]

soundFiles :: FindExpr
soundFiles
    = andExprSeq [ HasType IsFile
                 , matchExtRE "mp3|wav|midi"
                 ]

makeFiles :: FindExpr
makeFiles
    = andExprSeq [ HasType IsFile
                 , orExprSeq [ matchNameRE "[Mm]akefile"
                             , matchExtRE  "mk"
                             ]
                 ]

-- ------------------------------

reSpace                 :: Regex
reSpace                 = parseRegex "\\s"

reAsciiChar             :: Regex
reAsciiChar             = mkAlt reSpace $ mkSymRng '\33' '\127'

reLatin1Char            :: Regex
reLatin1Char            = mkAlt reAsciiChar reLatin1NoneAsciiChar

reLatin1NoneAsciiChar   :: Regex
reLatin1NoneAsciiChar   = mkSymRng '\160' '\255'

reUnicodeChar           :: Regex
reUnicodeChar           = mkAlt reLatin1Char reUnicodeNoneLatin1Char

reUnicodeNoneLatin1Char :: Regex
reUnicodeNoneLatin1Char = mkSymRng '\256' maxBound

reAsciiWord             :: Regex
reAsciiWord             = mkStar reAsciiChar

reLatin1Word            :: Regex
reLatin1Word            = mkStar reLatin1Char

reUnicodeWord           :: Regex
reUnicodeWord           = mkStar reUnicodeChar

reContainsTabs          :: Regex
reContainsTabs          = mkSeqs [mkAll, mkSym1 '\t', mkAll]

reParam                 :: Regex
reParam                 = mkSeqs [ mkSym1 '{'
                                 , mkRep 1 $ mkAlt (mkSym1 '-') (mkSymRng 'a' 'z')
                                 , mkSym1 '}'
                                 ]

reDirPath               :: Regex -> Regex
reDirPath re            = mkSeqs [ re
                                 , mkOpt $ mkSeq (mkSym1 '/') mkAll
                                 ]

-- ------------------------------

isAsciiText             :: String -> Bool
isAsciiText             = matchRE reAsciiWord

isLatin1Text            :: String -> Bool
isLatin1Text            = matchRE reLatin1Word

isUnicodeText           :: String -> Bool
isUnicodeText           = matchRE reUnicodeWord

containsLatin1          :: String -> Bool
containsLatin1          = matchRE $ mkSeqs [reAsciiWord,  reLatin1NoneAsciiChar, reLatin1Word]

containsNoneLatin1      :: String -> Bool
containsNoneLatin1      = matchRE $ mkSeqs [reLatin1Word, reUnicodeNoneLatin1Char, reUnicodeWord]

containsBinary          :: String -> Bool
containsBinary          = not . isLatin1Text

containsTabs            :: String -> Bool
containsTabs            = matchRE $ reContainsTabs

isUtfText               :: String -> Bool
isUtfText s             = not (isAsciiText s) && isUtf8 s

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
hasTrailingWS           = match ".*\\s"

hasTrailingWSLine       :: String -> Bool
hasTrailingWSLine       = any hasTrailingWS . lines

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

-- ----------------------------------------
--
-- edit functions

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

substToAsciiHaskell      :: String -> String
substToAsciiHaskell
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
    = unlines . map (concat . snd . mapAccumL rmTab 0) . lines
    where
    rmTab               :: Int -> Char -> (Int, String)
    rmTab i '\t'        = (i', replicate (i' - i) ' ')
                          where
                          i' = ((i + 8) `div` 8) * 8
    rmTab i ch          = (i+1, [ch])

-- ------------------------------

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

-- ------------------------------
