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

-- ------------------------------------------------------------

-- scans imgBase/imgDir for new pictures,
-- what's new is given by newerThan0, default is all from today since midnight

scanForNewImages :: Config -> IOE [(String, String, String)]
scanForNewImages c
    = do
      liftIO $ print [imgBase,imgBase',imgDir,imgDir',searchDir,newerThan0,filterEx]
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
      rawFilesFound <- mapM (findRaw imgBase') filesFound

      -- search for raw files in the parent dir
      xmpFilesFound <- mapM (findXmp imgBase') filesFound

      -- lookup date when image was shot
      imagesShot    <- CM.zipWithM findShot filesFound rawFilesFound

      -- sort files by shooting time and return
      return . map snd {- . sort -} . zip imagesShot
                 $ zip3 ( map normFn filesFound    )
                        ( rawFilesFound )
                        ( xmpFilesFound )
    where
    keyDateAndTime      = newAtom "exif:CreateDate"

    imgBase             = getImportBase                              c
    imgDir              = getDefOpt "."             "import-dir"     c
    newerThan0          = getDefOpt "1970-01-01"    "import-since"   c
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

    findShot f r
        | sortByShot
            = do
              imgAttrl <- allImgAttrs ["-CreateDate"] c imgBase f r ""
              return . fromMaybe "" . M.lookup keyDateAndTime $ imgAttrl
        | otherwise
            = return ""

    normFn f
        = drop (length imgBase' + 1) f

-- ------------------------------------------------------------
