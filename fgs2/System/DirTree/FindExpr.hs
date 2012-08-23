{- | module for evaluating find expressions -}

module System.DirTree.FindExpr
    ( module System.DirTree.FindExpr
    , matchRE
    )
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.Char                         ( isSpace )
import Data.List                         ( mapAccumL )
import Data.Maybe

import Text.Regex.XMLSchema.String      -- ( match, matchRE, parseRegex, isZero )

import Text.XML.HXT.Parser.XhtmlEntities ( xhtmlEntities )

import System.DirTree.Types
import System.DirTree.FileSystem

import qualified Data.Map                as M

-- ------------------------------

findExpr2FindPred :: FindExpr -> FindPred

findExpr2FindPred (MatchRE re) f
    = return $ matchRE re f

findExpr2FindPred (MatchExtRE re) f
    = return $ matchRE  (mkSeqs [mkAll, mkSym1 '.', re]) f

findExpr2FindPred (MatchPathRE re) f
    = pathName f >>= return . matchRE re

findExpr2FindPred FTrue f
    = truePred f

findExpr2FindPred FFalse f
    = falsePred f

findExpr2FindPred IsFile f
    = isFile f

findExpr2FindPred IsDir f
    = isDir f

findExpr2FindPred (HasCont p) f
    = isFile `andPred` (readFileContentsAsString >=> p) $ f

findExpr2FindPred (AndExpr e1 e2) f
    = andPred (findExpr2FindPred e1) (findExpr2FindPred e2) f

findExpr2FindPred (OrExpr e1 e2) f
    = orPred (findExpr2FindPred e1) (findExpr2FindPred e2) f

findExpr2FindPred (NotExpr e) f
    = not <$> findExpr2FindPred e f

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

-- ------------------------------

boringFiles     :: FindExpr
boringFiles
    = orExprSeq [ matchExtRE "bak|old|out|tmp|aux|log|[~]"
                , andExprSeq [ IsDir
                             , matchNameRE "cache|\\.xvpics"
                             ]
                , matchNameRE "\\.(directory|DS_Store)"
                , matchNameRE "[.]#.*"
                ]

cvsFiles        :: FindExpr
cvsFiles
    = orExprSeq [ matchPathRE ".*/CVS(/.*)?"
                , matchNameRE "\\.cvsignore"
                ]

badNames        :: FindExpr
badNames
    = andExprSeq [ NotExpr boringFiles,
                   matchNameRE ".*[^-+,._/a-zA-Z0-9].*"
                 ]

-- ------------------------------

imageFiles     :: FindExpr
imageFiles
    = andExprSeq [ IsFile
                 , matchExtRE "bmp|gif|jpg|jpeg|mov|nef|pbm|pgm|png|ppm|psd|rw2|tif|tiff|xmb|xcf"
                 ]

soundFiles :: FindExpr
soundFiles
    = andExprSeq [ IsFile
                 , matchExtRE "mp3|wav|midi"
                 ]

makeFiles :: FindExpr
makeFiles
    = andExprSeq [ IsFile
                 , orExprSeq [ matchNameRE    "[Mm]akefile"
                             , matchExtRE "mk"
                             ]
                 ]

-- ------------------------------

reSpace :: Regex
reSpace
    = parseRegex "\\s"

reAsciiChar :: Regex
reAsciiChar
    = mkAlt reSpace $ mkSymRng '\33' '\127'

reLatin1Char :: Regex
reLatin1Char
    = mkAlt reAsciiChar reLatin1NoneAsciiChar

reLatin1NoneAsciiChar :: Regex
reLatin1NoneAsciiChar
    = mkSymRng '\160' '\255'

reUnicodeChar :: Regex
reUnicodeChar
    = mkAlt reLatin1Char reUnicodeNoneLatin1Char

reUnicodeNoneLatin1Char :: Regex
reUnicodeNoneLatin1Char
    = mkSymRng '\256' maxBound

reAsciiWord :: Regex
reAsciiWord
    = mkStar reAsciiChar

reLatin1Word :: Regex
reLatin1Word
    = mkStar reLatin1Char

reUnicodeWord :: Regex
reUnicodeWord
    = mkStar reUnicodeChar

reContainsTabs :: Regex
reContainsTabs
    = mkSeqs [mkAll, mkSym1 '\t', mkAll]

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
