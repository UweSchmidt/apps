module Main where

import           Catalog.Cmd
import           Catalog.Options
import           Catalog.Sync
-- import           Data.ImageStore
-- import           Data.ImgTree
import           Data.Prim
import           System.Exit

main :: IO ()
main = mainWithArgs "sync" syncMain

syncMain :: Env -> IO ()
syncMain env = do
  res <- (^. _1) <$> runCmd (local (const env) syncCatalog)
  either (die . show) return res

syncCatalog :: Cmd ()
syncCatalog = do
  mountPath <- view envMountPath

  verbose $
    "syncCatalog: create archive at: " ++
    (mountPath ^. isoString . to show)

  initImgStore
    n'archive n'collections (mountPath </> s'photos)

  jsonPath <-
    (mountPath </>) <$>
    view envJsonArchive

  whenM (fileExist jsonPath) $ do
    verbose $ "read the current archive data from file " ++ show jsonPath
    loadImgStore jsonPath

  -- syncronize the image dir with the filesystem
  syncDir

  -- cleanup dead references
  -- cleanupAllCollections -- already done in syncDir

  -- verbose "syncCatalog: create the system collections"
  -- genSysCollections

  -- verbose "syncCatalog: create the collections per date"
  -- verbose "syncCatalog: TODO: skip this when updateCollectionsByDate is done"
  -- genCollectionsByDate

  verbose $ "syncCatalog: save state to " ++ show jsonPath
  saveImgStore jsonPath

  verbose "syncCatalog: sync finished"
