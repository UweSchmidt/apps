module Main where

import           Catalog.Cmd
import           Catalog.Sync
import           Data.ImageStore
import           Data.ImgTree
import           Data.Prim
import           System.Exit
import           System.IO
import qualified System.Posix as X

main :: IO ()
main = syncMain

syncMain :: IO ()
syncMain = do
  mountPath <- (</> "data") <$> X.getWorkingDirectory
  res <- (^. _1) <$> runCmd (local (envTrc .~ True) $ syncCatalog jsonPath mountPath)
  either
    (\ e -> do hPutStrLn stderr $ show e
               exitFailure
    )
    return
    res
  where
    jsonPath = "catalog1.json"

syncCatalog :: FilePath -> FilePath -> Cmd ()
syncCatalog jsonPath0 mountPath = do
  trc "create archive root"
  initImgStore n'archive n'collections (mountPath </> s'photos)

  let jsonPath = mountPath </> jsonPath0
  ex <- fileExist jsonPath
  when ex $ do
    trc $ "read the current archive data from file " ++ show jsonPath
    loadImgStore jsonPath

  trc "get the archive and the image root"
  refRoot <- use (theImgTree . rootRef)
  refImg  <- use (theImgTree . theNodeVal refRoot . theRootImgDir)

  trc "sync the archive with the file system"
  syncFS refImg

  trc "create the collections for the archive dirs"
  trc "and the collections per date"
  genCollectionRootMeta
  cleanupCollections
  genCollectionsByDir
  genCollectionsByDate

  trc $ "save state to " ++ show jsonPath
  saveImgStore jsonPath

  trc "sync finished"
