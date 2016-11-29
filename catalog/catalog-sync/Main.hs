module Main where

import           Catalog.Cmd
import           Catalog.Options
import           Catalog.Sync
import           Catalog.System.ImportPhoto2 ( cleanImportCols
                                             , loadImportData
                                             , insertImportPhoto2
                                             )
import           Catalog.Html.Cache ( fillImgCache )

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

  checkImgStore
  doIt jsonPath
  verbose "syncOrImportCatalog: sync finished"

doIt :: FilePath -> Cmd ()
doIt jp = do
  ip <- view envJsonImport
  case ip of
    Just i -> do
      importCols i
      saveImgStore jp

    Nothing -> do
      uc <- view envUpdateCache
      case uc of
        Just f ->
          updateCache f

        Nothing -> do
          syncDir
          checkImgStore
          saveImgStore jp


importCols :: FilePath -> Cmd ()
importCols f = do
  genSysCollections
  cleanImportCols
  d <- loadImportData f
  insertImportPhoto2 d
  checkImgStore

updateCache :: FilePath -> Cmd ()
updateCache f = do
  fillImgCache $ readPath p
  where
    p = ps'collections </> f
