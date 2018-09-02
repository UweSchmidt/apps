{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Cmd.Types
       ( module Control.Monad.ReaderStateErrIO
         -- Env
       , Env
       , mkEnv
       , CopyGeo
       , defaultEnv
       , envTrc
       , envVerbose
       , envJournal
       , envDryRun
       , envForceMDU
       , envPort
       , envJsonArchive
       , envJsonImport
       , envMountPath
       , envSyncDir
       , envFontName
       , envLogOp
       , envUpdateCache
       -- Cmd
       , Cmd
       , runCmd
       , runCmd'
       -- CmdMB
       , CmdMB
       , appMB
       , liftMB
       , pureMB
       , runMaybeT
       , runMB
       )
where

import           Control.Lens
import           Control.Monad.ReaderStateErrIO
import           Control.Monad.Trans.Maybe
import           Data.ImageStore
import           Data.Prim
import           System.IO

-- ----------------------------------------

data Env = Env
  { _trc         :: Bool
  , _verbose     :: Bool
  , _journal     :: Bool
  , _dryRun      :: Bool
  , _forceMDU    :: Bool  -- Meta Data Update
  , _port        :: Int
  , _jsonArchive :: FilePath
  , _jsonImport  :: Maybe FilePath
  , _mountPath   :: SysPath
  , _syncDir     :: FilePath
  , _fontName    :: Text
  , _logOp       :: String -> IO ()
  , _updateCache :: Maybe FilePath
  }

-- the named constructor
--
-- constructor and selector names are not exported

mkEnv :: Bool
      -> Bool
      -> Bool
      -> Bool
      -> Bool
      -> Int
      -> FilePath
      -> Maybe FilePath
      -> SysPath
      -> FilePath
      -> Text
      -> (String -> IO ())
      -> Maybe FilePath
      -> Env
mkEnv = Env

-- deriving instance Show Env

instance Config Env where
  traceOn   e = e ^. envTrc
  verboseOn e = e ^. envVerbose

type CopyGeo = ((Int, Int), AspectRatio)

defaultEnv :: Env
defaultEnv = Env
  { _trc          = False
  , _verbose      = False
  , _journal      = False
  , _dryRun       = False
  , _forceMDU     = False
  , _port         = 3001
  , _jsonArchive  = "catalog.json" -- rel to mount path
  , _jsonImport   = Nothing
  , _mountPath    = mkSysPath "."
  , _syncDir      = s'photos       -- the top archive dir
  , _fontName     = mempty
  , _logOp        = hPutStrLn stderr
  , _updateCache  = Nothing
  }

envTrc :: Lens' Env Bool
envTrc k e = (\ new -> e {_trc = new}) <$> k (_trc e)

envVerbose :: Lens' Env Bool
envVerbose k e = (\ new -> e {_verbose = new}) <$> k (_verbose e)

envJournal :: Lens' Env Bool
envJournal k e = (\ new -> e {_journal = new}) <$> k (_journal e)

envDryRun :: Lens' Env Bool
envDryRun k e = (\ new -> e {_dryRun = new}) <$> k (_dryRun e)

envForceMDU :: Lens' Env Bool
envForceMDU k e = (\ new -> e {_forceMDU = new}) <$> k (_forceMDU e)

envPort :: Lens' Env Int
envPort k e = (\ new -> e {_port = new}) <$> k (_port e)

envJsonArchive :: Lens' Env FilePath
envJsonArchive k e = (\ new -> e {_jsonArchive = new}) <$> k (_jsonArchive e)

envJsonImport :: Lens' Env (Maybe FilePath)
envJsonImport k e = (\ new -> e {_jsonImport = new}) <$> k (_jsonImport e)

envMountPath :: Lens' Env SysPath
envMountPath k e = (\ new -> e {_mountPath = new}) <$> k (_mountPath e)

envSyncDir :: Lens' Env FilePath
envSyncDir k e = (\ new -> e {_syncDir = new}) <$> k (_syncDir e)

envFontName :: Lens' Env Text
envFontName k e = (\ new -> e {_fontName = new}) <$> k (_fontName e)

envLogOp :: Lens' Env (String -> IO ())
envLogOp k e = (\ new -> e {_logOp = new}) <$> k (_logOp e)

envUpdateCache :: Lens' Env (Maybe FilePath)
envUpdateCache k e = (\ new -> e {_updateCache = new}) <$> k (_updateCache e)

-- ----------------------------------------

type Cmd = Action Env ImgStore

runCmd :: Cmd a -> IO (Either Msg a, ImgStore)
runCmd = runCmd' defaultEnv

runCmd' :: Env -> Cmd a -> IO (Either Msg a, ImgStore)
runCmd' env cmd = runAction cmd env emptyImgStore

type CmdMB = MaybeT Cmd

liftMB :: Cmd (Maybe a) -> CmdMB a
liftMB cmd = lift cmd >>= pureMB
{-# INLINE liftMB #-}

pureMB :: Maybe a -> CmdMB a
pureMB = maybe mzero return
{-# INLINE pureMB #-}

runMB :: Monoid a => CmdMB a -> Cmd a
runMB cmd = fromMaybe mempty <$> runMaybeT cmd

appMB :: (Monad m, Monoid b) => (a -> m b) -> (Maybe a -> m b)
appMB = maybe (return mempty)

-- ----------------------------------------
