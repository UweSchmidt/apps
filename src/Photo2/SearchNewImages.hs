module Photo2.SearchNewImages
where

import qualified Control.Monad as CM

import           Data.Atom
import           Data.Char
import qualified Data.Map as M
import           Data.Maybe
import           Data.List

import           Photo2.ArchiveTypes
import           Photo2.Config
import           Photo2.FilePath
import           Photo2.ImageOperations

import           System.FindGrepSed  ( FindExpr(..) )
import qualified System.FindGrepSed as F

import           System.Directory

import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

-- ------------------------------------------------------------

-- scans imgBase/imgDir for new pictures,
-- what's new is given by newerThan0, default is all from today since midnight

scanForNewImages :: Config -> IOE [(String, String, String)]
scanForNewImages c
    = do
      -- liftIO $ print [imgBase,imgBase',imgDir,imgDir',searchDir,newerThan0,filterEx]
      newerThan <- ( if null newerThan0
                     then ( do
                            currTime <- liftIO $ getTimeStamp
                            return . head . words $ currTime    -- today
                          )
                     else return newerThan0
                   )
      -- rename images to lowercase
      -- renameUpperCaseImageFiles dry searchDir (findExprTimeStamp newerThan)

      -- search the dir
      filesFound    <- liftIO $ F.find searchDir (findExpr newerThan)

      -- search for raw files in the parent dir
      rawFilesFound <- mapM findRaw filesFound

      -- search for raw files in the parent dir
      xmpFilesFound <- mapM findXmp filesFound

      -- lookup date when image was shot
      imagesShot    <- CM.zipWithM findShot filesFound rawFilesFound

      -- sort files by shooting time and return
      return . map snd {- . sort -} . zip imagesShot
                 $ zip3 ( map normFn filesFound    )
                        ( map normFn rawFilesFound )
                        ( map normFn xmpFilesFound )
    where
    keyDateAndTime      = newAtom "exif:CreateDate"

    imgBase             = getImportBase                              c
    imgDir              = getDefOpt "."             "import-dir"     c
    newerThan0          = getDefOpt "2008-01-01"    "import-since"   c
    filterEx            = getDefOpt ".*"            "import-pattern" c
    sortByShot          = optON                     "import-by-date" c

    imgBase'            = normPath imgBase
    imgDir'             = normPath imgDir

    searchDir
        | imgBase' `isPrefixOf` imgDir'
                        = imgDir
        | otherwise
                        = normPath (imgBase' </> imgDir')

    findExprTimeStamp ts
        = AndExpr [ IsFile
                  , FPred $ fileNewerThanDate ts
                  , if null filterEx
                    then FT
                    else RE filterEx
                  ]

    findExpr ts
        = AndExpr [ OrExpr [ Ext "jpg", Ext "jpeg"
                           , Ext "tif", Ext "tiff"
                           , Ext "gif"
                           , Ext "png"
                           ]
                  , findExprTimeStamp ts
                  ]

    findRaw f
        = liftIO $
          do
          ex <- doesFileExist fraw
          return ( if ex then fraw else "")
        where
        fn   = (`addExtension` "nef") . removeRawVersion . removeExtension . baseName $ f
        fraw = (</> fn) . dirPath . dirPath $ f

    findXmp f
        = liftIO $
          do
          ex <- doesFileExist fxmp
          return ( if ex then fxmp else "")
        where
        fn   = (`addExtension` "xmp") . removeExtension . baseName $ f
        fxmp = (</> fn) . dirPath . dirPath $ f

    removeRawVersion                                    -- remove raw conversion version 
        = sed remSubNo "([_a-zA-Z]+[0-9]+)-[0-9]+"
          where
          remSubNo = takeWhile (/= '-')

    findShot f r
        | sortByShot
            = do
              imgAttrl <- allImgAttrs ["-CreateDate"] c f r ""
              return . fromMaybe "" . M.lookup keyDateAndTime $ imgAttrl
        | otherwise
            = return ""

    normFn f
        = drop (length imgBase' + 1) f

-- ------------------------------------------------------------
