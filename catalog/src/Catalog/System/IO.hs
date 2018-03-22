{-# LANGUAGE DeriveFunctor #-}
-- all IO operations lifted to commands

module Catalog.System.IO
  ( SysPath
  , FileStatus
  , toSysPath
  , fileExist
  , dirExist
  , getFileStatus
  , getModiTime
  , getModiTime'
  , setModiTime
  , writeFileLB
  , writeFileT
  , readFileLB
  , readFileT
  , readFileT'
  , removeFile
  , renameFile
  , linkFile
  , createDir
  , removeDir
  , getWorkingDirectory
  , readDir
  , putStrLnLB
  , putStrLn'
  , atThisMoment
  , formatTimeIso8601
  , nowAsIso8601
  )
where

import           Catalog.Cmd.Types
import           Catalog.Cmd.Basic (SysPath, isoFilePath, toSysPath)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Prim.Prelude
import           Data.Prim.TimeStamp
import qualified Data.Text.IO     as T
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock  as C
import qualified Data.Time.Format as C
import qualified System.Directory as D
import qualified System.Posix     as X

-- ----------------------------------------

type FileStatus = X.FileStatus

fileExist :: SysPath -> Cmd Bool
fileExist sp = io . D.doesFileExist $ sp ^. isoFilePath

-- fileExist :: FilePath -> Cmd Bool
-- fileExist = io . D.doesFileExist

dirExist :: SysPath -> Cmd Bool
dirExist sp = io . D.doesDirectoryExist $ sp ^. isoFilePath

getFileStatus :: SysPath -> Cmd FileStatus
getFileStatus sp = io . X.getFileStatus $ sp ^. isoFilePath

getModiTime :: SysPath -> Cmd TimeStamp
getModiTime f = fsTimeStamp <$> getFileStatus f

getModiTime' :: SysPath -> Cmd TimeStamp
getModiTime' f = do
  ex <- fileExist f
  if ex
    then getModiTime f
    else return mempty

setModiTime :: TimeStamp -> SysPath -> Cmd ()
setModiTime ts sp =
  io $ X.setFileTimes (sp ^. isoFilePath) ep ep
  where
    ep = ts ^. isoEpochTime

writeFileLB :: SysPath -> LB.ByteString -> Cmd ()
writeFileLB sp = io . LB.writeFile (sp ^. isoFilePath)

readFileLB :: SysPath -> Cmd LB.ByteString
readFileLB sp = io . LB.readFile $ sp ^. isoFilePath

readFileT :: SysPath -> Cmd Text
readFileT sp = io . T.readFile $ sp ^. isoFilePath

readFileT' :: SysPath -> Cmd Text
readFileT' fp = do
  ex <- fileExist fp
  if ex
    then readFileT fp
    else return mempty

writeFileT :: SysPath -> Text -> Cmd ()
writeFileT sp = io . T.writeFile (sp ^. isoFilePath)

removeFile :: SysPath -> Cmd ()
removeFile sp = io . D.removeFile $ sp ^. isoFilePath

renameFile :: SysPath -> SysPath -> Cmd ()
renameFile old new = io $ X.rename (old ^. isoFilePath) (new ^. isoFilePath)

-- try to make a hard link, if that fails copy file

linkFile :: SysPath -> SysPath -> Cmd ()
linkFile oldf newf =
  (io $ X.createLink old new)
  `catchError`
  (\ _e -> io $ D.copyFile old new)
  where
    old = oldf ^. isoFilePath
    new = newf ^. isoFilePath

createDir :: SysPath -> Cmd ()
createDir sp = io . D.createDirectoryIfMissing True $ sp ^. isoFilePath

removeDir :: SysPath -> Cmd ()
removeDir sp = io . D.removeDirectoryRecursive $ sp ^. isoFilePath

getWorkingDirectory :: Cmd FilePath
getWorkingDirectory = io X.getWorkingDirectory

readDir :: SysPath -> Cmd [FilePath]
readDir sp = io $ do
  s  <- X.openDirStream (sp ^. isoFilePath)
  xs <- readDirEntries s
  X.closeDirStream s
  return xs
  where
    readDirEntries s = do
      e1 <- X.readDirStream s
      if null e1
        then return []
        else do
          es <- readDirEntries s
          return (e1 : es)

putStrLnLB :: LB.ByteString -> Cmd ()
putStrLnLB = io . LB.putStrLn

putStrLn' :: String -> Cmd ()
putStrLn' = io . putStrLn

-- ----------------------------------------

atThisMoment :: Cmd UTCTime
atThisMoment = io C.getCurrentTime

formatTimeIso8601 :: UTCTime -> String
formatTimeIso8601 =
  C.formatTime C.defaultTimeLocale (C.iso8601DateFormat (Just "%H:%M:%S"))

nowAsIso8601 :: Cmd String
nowAsIso8601 = formatTimeIso8601 <$> atThisMoment

-- ----------------------------------------
