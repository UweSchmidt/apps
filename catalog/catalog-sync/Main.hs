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
  jsonPath0 <- view envJsonArchive
  mountPath <- view envMountPath

  verbose "create archive root"
  initImgStore n'archive n'collections (mountPath </> s'photos)

  let jsonPath = mountPath </> jsonPath0
  ex <- fileExist jsonPath
  when ex $ do
    verbose $ "read the current archive data from file " ++ show jsonPath
    loadImgStore jsonPath

  verbose "get the archive and the image root"
  refRoot <- use (theImgTree . rootRef)
  refImg  <- use (theImgTree . theNodeVal refRoot . theRootImgDir)

  verbose "sync the archive with the file system"
  syncFS refImg

  verbose "create the collections for the archive dirs"
  verbose "and the collections per date"
  genCollectionRootMeta
  cleanupCollections
  genClipboardCollection
  genTrashCollection
  genCollectionsByDir
  genCollectionsByDate

  verbose $ "save state to " ++ show jsonPath
  saveImgStore jsonPath

  verbose "sync finished"
