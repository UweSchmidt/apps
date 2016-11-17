module Main where

import           Catalog.Cmd
import           Catalog.Options
import           Catalog.Sync
import           Catalog.System.ImportPhoto2 ( cleanImportCols
                                             , loadImportData
                                             , insertImportPhoto2
                                             )
-- import           Data.ImageStore
-- import           Data.ImgTree
import           Data.Prim
import           System.Exit

main :: IO ()
main = mainWithArgs "sync" syncMain

syncMain :: Env -> IO ()
syncMain env = do
  res <- (^. _1) <$> runCmd (local (const env) syncOrImportCatalog)
  either (die . show) return res

syncOrImportCatalog :: Cmd ()
syncOrImportCatalog = do
  mountPath <- view envMountPath

  verbose $
    "syncOrImportCatalog: create archive at: " ++
    (mountPath ^. isoString . to show)

  initImgStore
    n'archive n'collections (mountPath </> s'photos)

  jsonPath <-
    (mountPath </>) <$>
    view envJsonArchive

  whenM (fileExist jsonPath) $ do
    verbose $ "read the current archive data from file " ++ show jsonPath
    loadImgStore jsonPath

  -- if import file is set, do an import else sync an image dir
  view envJsonImport >>=
    maybe syncDir importCols

  verbose $ "syncOrImportCatalog: save state to " ++ show jsonPath
  saveImgStore jsonPath

  verbose "syncOrImportCatalog: sync finished"

importCols :: FilePath -> Cmd ()
importCols f = do
  genSysCollections
  cleanImportCols
  d <- loadImportData f
  insertImportPhoto2 d
