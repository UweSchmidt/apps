{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Zip
where

import Catalog.Cmd
import Catalog.Html.Basic     ( buildImgPath )
import Catalog.System.Convert ( genImage )
import Data.ImageStore
import Data.ImgTree
import Data.Prim

-- ----------------------------------------

zipCollection' :: Path -> Cmd FilePath
zipCollection' p = do
  verbose $ "zipCollection: " ++ quotePath p
  mbi <- lookupByPath p
  maybe
    (abort $ "zipCollection: illegal path " ++ p ^. isoString)
    (uncurry zipCollection) mbi

zipCollection :: ObjId -> ImgNode -> Cmd FilePath
zipCollection i e
  | isCOL e = do
      trcObj i "zipColl: create zip archive"
      p  <- objid2path i
      mp <- use theMountPath

      let archDir      = mp ++ ps'zipcache ++ p ^. isoString
      let archFilePath = archDir ++ ".zip"

      trcObj i $ "zipColl: create zip archive " ++ archFilePath ++ " for collection " ++ show p

      -- remove old stuff
      whenM (fileExist archFilePath) $ removeFile archFilePath
      whenM (dirExist archDir)  $ removeDir archDir

      -- create temporary dir for archive entries
      trc $ "zipCollection: create temporary dir " ++ archDir
      createDir archDir

      -- recursive traversal of the collection to create image copies
      trc $ "zipCollection: zip entries"
      zipEntries archDir e

      -- create archive
      trc $ "zipCollection: zip archive " ++ archDir
      zipArchive archDir archFilePath

      -- cleanup: remove temporary dir
      trc $ "zipCollection: remove temporary dir " ++ archDir
      removeDir archDir

      return archFilePath

  | otherwise = do
      abort $  "zipCollection: not a collection"


zipEntries :: FilePath -> ImgNode -> Cmd ()
zipEntries px e =
  mapM_ (uncurry zipE) $ zip cs [0..]
  where
    cs :: [ColEntry]
    cs = e ^. theColEntries

    zipE :: ColEntry -> Int -> Cmd ()
    zipE ce i = colEntry zipI zipC ce
      where
        i' = fmtInt (i + 1)

        zipI :: ObjId -> Name -> Cmd ()
        zipI oid n = do
          srcPath <- buildImgPath oid n
          cpyPath <- genImage $ "/" ++ geo ^. isoString ++ srcPath
          trc $ "zipEntries: ln " ++ cpyPath ++ " " ++ lnk
          linkFile cpyPath lnk
            where
              geo = geoar'org ^. isoString
              lnk = px </> i' ++ ".jpg"

        zipC :: ObjId -> Cmd ()
        zipC oid = do
          createDir px'
          getImgVal oid >>= zipEntries px'
            where
              px' = px </> i'

fmtInt :: Int -> String
fmtInt i = s0 ++ si
  where
    si = show i
    s0 = replicate (3 - length si) '0'

-- ----------------------------------------
--
-- the system call to zip

zipArchive :: FilePath -> FilePath -> Cmd ()
zipArchive dir file =
  void $
    execProcess "bash" [] $
    unwords ["cd", baseDir, ";"
            , "zip", "-v", "-r", archive, archDir
            ]
  where
    baseDir = takeDirectory dir
    archive = takeFileName  file
    archDir = takeFileName  dir

-- ----------------------------------------
