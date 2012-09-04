module System.DirTree.FileSystem
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.String.Unicode              ( utf8ToUnicode
                                        , unicodeToUtf8
                                        )

import System.Directory
import System.IO                        ( IOMode(..)
                                        , openFile
                                        , hClose
                                        , hPutStrLn
                                        , stderr
                                        )
import System.Posix.Files
import System.Posix.User

import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C

import System.DirTree.FilePath
import System.DirTree.Types

-- ----------------------------------------
--
-- module System.DirTree.FileSystem
--
-- ----------------------------------------

ignoreErrors :: Cmd a -> Cmd a
ignoreErrors
    = local $ \ e -> e {theErrorFlag = False}

withDefault :: Cmd a -> Cmd a -> Cmd a
withDefault c1 c2
    = ignoreErrors c1 `orElse` c2

msg :: String -> FilePath -> Cmd ()
msg s f
    = asks warningOn
      `guards` (do p <- pathName f
                   io $ hPutStrLn stderr $ s ++ ": " ++ p
               )

cd :: FilePath -> Cmd ()
cd "."
    = return ()
cd p
    = do io . setCurrentDirectory $ p
         pwd >>= trc . ("cwd " ++) . show

pwd :: Cmd String
pwd
    = io getCurrentDirectory

pathName :: FilePath -> Cmd FilePath
pathName name
    = (`joinFile` name) <$> asks theCwd

-- file status tests

isDir :: FindPred
isDir = isEntryType isDirectory

isFile :: FindPred
isFile = isEntryType isRegularFile

isSymLink :: FindPred
isSymLink = isEntryType isSymbolicLink

isCharDev :: FindPred
isCharDev = isEntryType isCharacterDevice

isBlockDev :: FindPred
isBlockDev = isEntryType isBlockDevice

isPipe :: FindPred
isPipe = isEntryType isNamedPipe

isSock :: FindPred
isSock = isEntryType isSocket

isEntryType :: (FileStatus -> Bool) -> FindPred
isEntryType isType f
    = do getStatus <- (\ b -> if b
                              then getFileStatus
                              else getSymbolicLinkStatus
                      ) <$> asks theFollowSymlink

         (isType <$> (io $ getStatus f))
         `withDefault` return False

-- return the file modes as string in format like ls -l

getFileMode :: FilePath -> Cmd String
getFileMode f
    = do fm <- io $ fileMode <$> getSymbolicLinkStatus f
         trc $ unwords ["getFileMode: mode for", show f, "is", show $ showMode fm]
         return $ showMode fm
    where
      showMode m
          = zipWith showM ms cs
          where
            ms = [ ownerReadMode, ownerWriteMode, ownerExecuteMode
                 , groupReadMode, groupWriteMode, groupExecuteMode
                 , otherReadMode, otherWriteMode, otherExecuteMode
                 ]
            cs = "rwxrwxrwx"
            showM m' c'
                | m `intersectFileModes` m' == m' = c'
                | otherwise                       = '-'

-- return the file owner name or the owner id as string

getFileOwner :: FilePath -> Cmd String
getFileOwner f
    = do oid <- io $ fileOwner <$> getSymbolicLinkStatus f
         (io $ userName <$> getUserEntryForID oid)
           `withDefault` (return . show $ oid)

-- return the file group name or the group id as string

getFileGroup :: FilePath -> Cmd String
getFileGroup f
    = do gid <- io $ fileGroup <$> getSymbolicLinkStatus f
         (io $ groupName <$> getGroupEntryForID gid)
           `withDefault` (return . show $ gid)

getFileContents :: FilePath -> Cmd String
getFileContents f
    = do trc $ "getFileContents: reading file " ++ show f
         readFileContents f >>= decodeFileContents

getDirContents :: FilePath -> Cmd [String]
getDirContents f
    = do trc $ "getDirContents: reading dir " ++ show f
         io . getDirectoryContents $ f

decodeFileContents :: ByteString -> Cmd String
decodeFileContents contents
    = do dec <- asks (decFct . theUtf8DecFlag)
         let (res, es) = dec . C.unpack $ contents
         if null es
            then return res
            else abort "UTF8 decoding errors detected"
    where
      decFct True  = utf8ToUnicode
      decFct False = \ x -> (x, [])

encodeFileContents :: String -> Cmd ByteString
encodeFileContents contents
    = do enc <- asks (encFct . theUtf8EncFlag)
         return $ C.pack . enc  $ contents
    where
      encFct True  = unicodeToUtf8
      encFct False = id

editFileContents :: (String -> String) -> FilePath -> Cmd ()
editFileContents editFct f
    = do trc $ "editFileContents: edit file " ++ show f
         bc  <- readFileContents f
         sc  <- decodeFileContents bc
         bc' <- encodeFileContents . editFct $ sc
         when (bc' /= bc) $
           do trc $ "editFileContents: contents changed in " ++ show f
              asks theCreateBackup
                `guards`
                ( do bf <- asks (($ f) . theBackupName)
                     trc $ "editFileContents: create backup file " ++ show bf
                     writeFileContents bf bc
                )
              trc $ "editFileContents: write canged contents back to " ++ show f
              msg "contents edited in file" f
              writeFileContents f bc'

writeFileContents :: FilePath -> ByteString -> Cmd ()
writeFileContents f' b'
    = io $ do h <- openFile f' WriteMode
              B.hPutStr h b'
              hClose h

writeStringAsFileContents :: FilePath -> String -> Cmd ()
writeStringAsFileContents f' s'
    = writeFileContents f' $ C.pack s'

readFileContents :: FilePath -> Cmd ByteString
readFileContents f
    = io $ do h <- openFile f ReadMode
              b <- B.hGetContents h
              b `seq` hClose h
              return b

readFileContentsAsString :: FilePath -> Cmd String
readFileContentsAsString f
    = C.unpack <$> readFileContents f

rmFile :: FilePath -> Cmd ()
rmFile f
    = io $
      do x <- doesFileExist f
         when x $ removeFile f

-- ----------------------------------------
