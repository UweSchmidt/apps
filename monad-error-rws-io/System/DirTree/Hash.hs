module System.DirTree.Hash
    ( genChecksumProcessor
    , sha1Hash
    , md5Hash
    )
where

import Control.Applicative
import Control.Arrow                         ( first
                                             , second
                                             , (&&&)
                                             , (***)
                                             )
import Control.Monad.RWSErrorIO

import Data.Digest.Pure.SHA
import Data.Digest.OpenSSL.MD5
import Data.IORef
import Data.List                             ( partition
                                             , (\\)
                                             )
import System.DirTree.Types
import System.DirTree.FileSystem

import Text.Regex.XMLSchema.String	     ( match )

import qualified Data.Map                    as M
import qualified Data.ByteString.Lazy        as L

-- ----------------------------------------

type HashDict      = M.Map FilePath Hash

type ChecksumState = (HashDict, HashDict)

type ChecksumRef   = IORef ChecksumState

-- ----------------------------------------

genChecksumProcessor :: Cmd (Cmd (), FilePath -> Cmd (), Cmd ())
genChecksumProcessor
    = do dictRef <- newChecksumRef
         csFile  <- asks theChecksumFile
         return ( initDict csFile >>= initChecksumState dictRef
                , checkCmd  dictRef
                , finishCmd csFile dictRef
                )

-- ----------------------------------------

sha1Hash           :: HashFct
sha1Hash           = showDigest . sha1 . L.fromChunks . (:[])

md5Hash            :: HashFct
md5Hash            = last . words . md5sum

-- ----------------------------------------
--
-- basic IORef commands

newChecksumRef     :: Cmd ChecksumRef
newChecksumRef
    = io $ newIORef (M.empty, M.empty)

lookupOldHash      :: FilePath -> ChecksumRef -> Cmd (Maybe Hash)
lookupOldHash f r
    = M.lookup f . fst <$> (io $ readIORef r)

insertNewHash      :: FilePath -> Hash -> ChecksumRef -> Cmd ()
insertNewHash f h r
    = io $ modifyIORef r $ second $ M.insert f h

initChecksumState  :: ChecksumRef -> HashDict -> Cmd ()
initChecksumState r oldDict
    = io $ writeIORef r (oldDict, M.empty)

getChecksumState :: ChecksumRef -> Cmd ChecksumState
getChecksumState
    = io . readIORef

-- ----------------------------------------

-- read hashes from file

initDict :: FilePath -> Cmd HashDict
initDict csFile
    = do (csl, rest) <- parseChecksumContents <$>
                        ( do x <- isFile csFile
                             if x
                                then readFileContentsAsString csFile
                                else return ""
                        )
         when (not . null $ rest)
                  $ do p <- pathName csFile
                       warn $ "some lines in checksum file don't match format" ++ show p ++ "\n" ++ unlines rest
         return $ M.fromList csl
    where
      parseChecksumContents
          = first (map toHash) . partition (match csLines) . lines
          where
            csLines  = "[0-9a-f]+[ ]+([*][ ]+)?.*"
            toHash s = (fn, h)
                where
                  (h, s1) = span (`elem` "0123456789ABCDEFabcdef") s
                  fn      = dropWhile (`elem` " *") s1

-- compute hashes and compare with old hashes

checkCmd :: ChecksumRef -> FilePath -> Cmd ()
checkCmd dictRef f
          = do trc $ "computing hash for file " ++ show f
               newHash <- sha1Hash <$> readFileContents f
               h       <- lookupOldHash f dictRef
               case h of
                 Nothing
                     -> return () -- out "file not yet in hash dictionary" f
                 Just oldHash
                     -> do when (oldHash /= newHash) $
                                out "hash has changed for file" f
               insertNewHash f newHash dictRef

-- check removed and new files and update checksum file

finishCmd :: FilePath -> ChecksumRef -> Cmd ()
finishCmd csFile dictRef
    = do s@(_old, new) <- getChecksumState dictRef
         let (removedFiles, newFiles)
                 = (uncurry (\\) &&& uncurry (flip (\\))) . (M.keys *** M.keys) $ s

         when (not . null $ removedFiles) $
              mapM_ (out "file not found for hash in checksum file") removedFiles

         when (not . null $ newFiles) $
              mapM_ (out $ "hash not found in current checksum file") newFiles 

         update <- asks theHashUpdate
         when update $
              if M.null new
              then rmFile csFile
              else writeStringAsFileContents csFile $ format new
    where
      format
          = unlines . map (\ (f, h) -> h ++ " * " ++ f) . M.toList

out :: String -> FilePath -> Cmd ()
out msg f
    = do p <- pathName f
         io $ putStrLn $ unwords ["check-hashes:", msg ++ ":", p]


-- ----------------------------------------
