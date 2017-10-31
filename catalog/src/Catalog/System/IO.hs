-- all IO operations lifted to commands

module Catalog.System.IO
where

import           Catalog.Cmd.Types
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

fileExist :: FilePath -> Cmd Bool
fileExist = io . D.doesFileExist

dirExist :: FilePath -> Cmd Bool
dirExist = io . D.doesDirectoryExist

getFileStatus :: FilePath -> Cmd FileStatus
getFileStatus = io . X.getFileStatus

getModiTime :: FilePath -> Cmd TimeStamp
getModiTime f = fsTimeStamp <$> getFileStatus f

getModiTime' :: FilePath -> Cmd TimeStamp
getModiTime' f = do
  ex <- fileExist f
  if ex
    then getModiTime f
    else return mempty

writeFileLB :: FilePath -> LB.ByteString -> Cmd ()
writeFileLB f = io . LB.writeFile f

readFileLB :: FilePath -> Cmd LB.ByteString
readFileLB = io . LB.readFile

readFileT :: FilePath -> Cmd Text
readFileT = io . T.readFile

writeFileT :: FilePath -> Text -> Cmd ()
writeFileT f = io . T.writeFile f

removeFile :: FilePath -> Cmd ()
removeFile = io . D.removeFile

renameFile :: FilePath -> FilePath -> Cmd ()
renameFile old new = io $ X.rename old new

-- try to make a hard link, if that fails copy file

linkFile :: FilePath -> FilePath -> Cmd ()
linkFile old new =
  (io $ X.createLink old new)
  `catchError`
  (\ _e -> io $ D.copyFile old new)

createDir :: FilePath -> Cmd ()
createDir = io . D.createDirectoryIfMissing True

removeDir :: FilePath -> Cmd ()
removeDir = io . D.removeDirectoryRecursive

getWorkingDirectory :: Cmd FilePath
getWorkingDirectory = io X.getWorkingDirectory

readDir :: FilePath -> Cmd [FilePath]
readDir p = io $ do
  s  <- X.openDirStream p
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
