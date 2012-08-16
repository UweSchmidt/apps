module System.DirTree.FindExpr
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.List                        ( isSuffixOf )

import Text.Regex.XMLSchema.String	( match )

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
    = return $ match re f

findExpr2FindPred (MatchExtRE re) f
    = return $ match (".*[.](" ++ re ++ ")") f

findExpr2FindPred (MatchPathRE re) f
    = pathName f >>= return . match re

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

findExpr2FindPred (AndExpr fl) f
    = (foldl andPred truePred . map findExpr2FindPred) fl f

findExpr2FindPred (OrExpr fl) f
    = (foldl orPred falsePred . map findExpr2FindPred) fl f

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

boringFiles     :: FindExpr
boringFiles
    = OrExpr [ MatchExtRE "bak|old|out|tmp|aux|log|[~]"
             , AndExpr [ IsDir
                       , OrExpr [ Name "cache"
                                , Name ".xvpics"
                                ]
                       ]
             , Name ".directory"
             , Name ".DS_Store"
             , MatchRE "[.]#.*"
             ]

cvsFiles        :: FindExpr
cvsFiles
    = OrExpr [ MatchPathRE ".*/CVS(/.*)?"
             , Name ".cvsignore"
             ]

badNames        :: FindExpr
badNames
    = AndExpr [ NotExpr boringFiles,
                MatchRE ".*[^-+,._/a-zA-Z0-9].*"
              ]

-- ------------------------------

imageFiles     :: FindExpr
imageFiles
    = AndExpr [ IsFile
              , MatchExtRE "bmp|gif|jpg|jpeg|mov|nef|pbm|pgm|png|ppm|psd|rw2|tif|tiff|xmb|xcf"
              ]

soundFiles :: FindExpr
soundFiles
    = AndExpr [ IsFile
              , MatchExtRE "mp3|wav|midi"
              ]

makeFiles :: FindExpr
makeFiles
    = AndExpr [ IsFile
              , OrExpr [ MatchRE "[Mm]akefile"
                       , MatchExtRE "mk"
                       ]
              ]

-- ------------------------------
