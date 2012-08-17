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
                                        )
import System.Posix.Files

import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C

import System.DirTree.FilePath
import System.DirTree.Types

-- ----------------------------------------
--
-- module System.DirTree.FileSystem
--
-- ----------------------------------------

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

isDir :: FilePath -> Cmd Bool
isDir f
    = do s <- io $ getSymbolicLinkStatus f 	-- followSymLinks: getFileStatus
         return (isDirectory s)

isFile :: FilePath -> Cmd Bool
isFile f
    = do s <- io $ getSymbolicLinkStatus f 	-- followSymLinks: getFileStatus
         return (isRegularFile s)

getFileContents :: FilePath -> Cmd String
getFileContents f
    = do trc $ "getFileContents: reading file " ++ show f
         dec       <- decFct <$> asks theUtf8Flag
         (res, es) <- (dec . C.unpack) <$> readFileContents f
         if null es
            then return res
            else abort "UTF8 decoding errors detected"
    where
      decFct True  = utf8ToUnicode
      decFct False = \ x -> (x, [])

getDirContents :: FilePath -> Cmd [String]
getDirContents f
    = do trc $ "getDirContents: reading dir " ++ show f
         io . getDirectoryContents $ f


editFileContents :: (String -> String) -> FilePath -> Cmd ()
editFileContents ef f
    = do trc $ "editFileContents: edit file " ++ show f
         bc <-io $ do h <- openFile f ReadMode
                      c <- B.hGetContents h
                      c `seq` hClose h
                      return c
         let bc' = C.pack . ef . C.unpack $ bc
         when (bc' /= bc) $
           do trc $ "editFileContents: contents changed in " ++ show f
              asks theCreateBackup
                `guards`
                ( do bf <- asks (($ f) . theBackupName) 
                     trc $ "editFileContents: create backup file " ++ show bf
                     writeFileContents bf bc
                )
              trc $ "editFileContents: write canged contents back to " ++ show f
              writeFileContents f bc'

writeFileContents :: FilePath -> B.ByteString -> Cmd ()
writeFileContents f' b'
    = io $ do h <- openFile f' WriteMode
              B.hPutStr h b'
              hClose h

readFileContents :: FilePath -> Cmd B.ByteString
readFileContents f
    = io $ do h <- openFile f ReadMode
              b <- B.hGetContents h
              b `seq` hClose h
              return b

-- ----------------------------------------
