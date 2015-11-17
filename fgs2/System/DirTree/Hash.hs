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
import Data.Digest.Pure.MD5                  ( md5 )
import Data.IORef
import Data.List                             ( partition
                                             , (\\)
                                             )
import System.DirTree.Types
import System.DirTree.FileSystem

import Text.Regex.XMLSchema.String           ( match )

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
         csFct   <- asks theHashFct
         csUpd   <- asks theHashUpdate
         return ( initDict csFile >>= initChecksumState dictRef
                , checkCmd  csFct  csUpd dictRef
                , finishCmd csFile csUpd dictRef
                )

-- ----------------------------------------

sha1Hash           :: HashFct
sha1Hash           = showDigest . sha1 . L.fromChunks . (:[])

md5Hash            :: HashFct
md5Hash            = show . md5 . L.fromStrict

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

checkCmd :: HashFct -> Bool -> ChecksumRef -> FilePath -> Cmd ()
checkCmd hashFct update dictRef f
    | update
        = do trc $ "updating hash for file " ++ show f
             newHash <- hashFct <$> readFileContents f
             h       <- length newHash `seq` lookupOldHash f dictRef
             case h of
               Nothing
                   -> trc $ "file not yet in hash dictionary: " ++ show f
               Just oldHash
                   -> do when (oldHash /= newHash) $
                              msg "hash has changed for file" f
             insertNewHash f newHash dictRef
    | otherwise
        = do trc $ "checking hash for file " ++ show f 
             h <- lookupOldHash f dictRef
             case h of
               Nothing
                   -> trc $ "file not yet in hash dictionary: " ++ show f
               Just oldHash
                   -> do newHash <- hashFct <$> readFileContents f
                         when (oldHash /= newHash) $
                              msg "hash has changed for file" f

-- check removed and new files and update checksum file

finishCmd :: FilePath -> Bool -> ChecksumRef -> Cmd ()
finishCmd csFile update dictRef
    | update
        = do s@(_old, new) <- getChecksumState dictRef
             let (removedFiles, newFiles)
                     = (uncurry (\\) &&& uncurry (flip (\\))) . (M.keys *** M.keys) $ s

             when (not . null $ removedFiles) $
                  mapM_ (msg "file not found for hash in checksum file") removedFiles

             when (not . null $ newFiles) $
                  mapM_ (msg $ "hash not found in current checksum file") newFiles

             if M.null new
                then rmFile csFile
                else writeStringAsFileContents csFile $ format new
    | otherwise
        = return ()
    where
      format
          = unlines . map (\ (f, h) -> h ++ " * " ++ f) . M.toList

-- ----------------------------------------
