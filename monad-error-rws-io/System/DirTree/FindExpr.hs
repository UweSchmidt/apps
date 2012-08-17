module System.DirTree.FindExpr
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.List                        ( isPrefixOf, isSuffixOf )

import Text.Regex.XMLSchema.String	-- ( match, matchRE, parseRegex, isZero )

import System.DirTree.Types
import System.DirTree.FileSystem

-- ------------------------------

findExpr2FindPred :: FindExpr -> FindPred

findExpr2FindPred (FPred p) f
    = p f

findExpr2FindPred (Ext ext) f
    = return $ ext `isSuffixOf` f

findExpr2FindPred (Name f1) f
    = return $ f1 == f

findExpr2FindPred (PathName f1) f
    = (== f1) <$> pathName f

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
    = isFile `andPred` (getFileContents >=> p) $ f

findExpr2FindPred (AndExpr2 e1 e2) f
    = andPred (findExpr2FindPred e1) (findExpr2FindPred e2) f

findExpr2FindPred (OrExpr2 e1 e2) f
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

parseContextRegex :: (String -> Regex) -> String -> Regex
parseContextRegex parseRegex' re
    = re'
    where
      re' = mkSeqs . concat $ [ startContext
                              , (:[]) . parseRegex' $ re2
                              , endContext
                              ]
      (startContext, re1)
          | "^"   `isPrefixOf` re   = ([],                          tail   re)
          | "\\<" `isPrefixOf` re   = ([parseRegexExt "(\\A\\W)?"], drop 2 re)
          | otherwise               = ([mkStar mkDot],                     re)
      (endContext, re2)
          | "$"   `isSuffixOf` re1  = ([],                          init          re1)
          | "\\>" `isSuffixOf` re1  = ([parseRegexExt "(\\W\\A)?"], init . init $ re1)
          | otherwise               = ([mkStar mkDot],                            re1)

-- ------------------------------

boringFiles     :: FindExpr
boringFiles
    = orExpr [ matchExtRE "bak|old|out|tmp|aux|log|[~]"
             , andExpr [ IsDir
                       , orExpr [ Name "cache"
                                , Name ".xvpics"
                                ]
                       ]
             , Name ".directory"
             , Name ".DS_Store"
             , matchNameRE "[.]#.*"
             ]

cvsFiles        :: FindExpr
cvsFiles
    = orExpr [ matchPathRE ".*/CVS(/.*)?"
             , Name ".cvsignore"
             ]

badNames        :: FindExpr
badNames
    = andExpr [ NotExpr boringFiles,
                matchNameRE ".*[^-+,._/a-zA-Z0-9].*"
              ]

-- ------------------------------

imageFiles     :: FindExpr
imageFiles
    = andExpr [ IsFile
              , matchExtRE "bmp|gif|jpg|jpeg|mov|nef|pbm|pgm|png|ppm|psd|rw2|tif|tiff|xmb|xcf"
              ]

soundFiles :: FindExpr
soundFiles
    = andExpr [ IsFile
              , matchExtRE "mp3|wav|midi"
              ]

makeFiles :: FindExpr
makeFiles
    = andExpr [ IsFile
              , orExpr [ matchNameRE    "[Mm]akefile"
                       , matchExtRE "mk"
                       ]
              ]

-- ------------------------------

star, plus              :: String -> String

star s                  = "(" ++ s ++ ")*"
plus s                  = "(" ++ s ++ ")+"

reAsciiChar
 , reLatin1Char, reLatin1'Char
 , reUnicodeChar, reUnicode'Char
 , reNoTabsChar         :: String

reAsciiChar             = "[\\s\33-\127]"
reLatin1Char            = "[\\s\33-\127\160-\255]"
reLatin1'Char           = "[\160-\255]"
reUnicodeChar           = "[\\s\33-\127\160-" ++ [maxBound::Char] ++ "]"
reUnicode'Char          = "[\256-" ++ [maxBound::Char] ++ "]"
reNoTabsChar            = "[\\n\\r\32-\127\160-" ++ [maxBound::Char] ++ "]"

isAsciiText             :: String -> Bool
isAsciiText             = match $ star reAsciiChar

isLatin1Text            :: String -> Bool
isLatin1Text            = match $ star reLatin1Char

isUnicodeText           :: String -> Bool
isUnicodeText           = match $ star reUnicodeChar

containsLatin1          :: String -> Bool
containsLatin1          = match $ plus ((star reAsciiChar) ++ reLatin1'Char)

containsNoneLatin1      :: String -> Bool
containsNoneLatin1      = match $ plus ((star reLatin1Char) ++ reUnicode'Char)

containsBinary          :: String -> Bool
containsBinary          = not . isLatin1Text

containsTabs            :: String -> Bool
containsTabs            = match $ plus ((star reNoTabsChar) ++ "\\t")

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

hasTrailingWSLine	:: String -> Bool
hasTrailingWSLine	= any hasTrailingWS . lines

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
