module Main where

import           Catalog.Cmd
import           Catalog.Options
import           Catalog.Sync
import           Data.ImageStore
import           Data.ImgTree
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

  -- verbose "syncCatalog: get the archive and the image root"
  -- refRoot  <- use (theImgTree . rootRef)
  -- refImg   <- use (theImgTree . theNodeVal refRoot . theRootImgDir)

  syncPath <-
    (n'archive `consPath`) . readPath . ("/" ++) <$>
    view envSyncDir
  verbose $
    "syncCatalog: sync the archive dir with the file system: " ++
    (syncPath ^. isoString . to show)
  syncDir syncPath
  -- syncFS refImg  -- old

  cleanupCollections

  verbose "syncCatalog: create the system collections"
  genSysCollections

  verbose "syncCatalog: create the collections for the archive dirs"
  genCollectionsByDir

  verbose "syncCatalog: create the collections per date"
  genCollectionsByDate

  verbose $ "syncCatalog: save state to " ++ show jsonPath
  saveImgStore jsonPath

  verbose "syncCatalog: sync finished"
