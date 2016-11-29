{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Cmd.Types
       ( module Catalog.Cmd.Types
       , module Control.Monad.RWSErrorIO
       , module Control.Monad.Except
       )
where

import           Control.Concurrent.QSem
import           Control.Exception.Base (bracket_)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.Prim
import           System.IO

-- ----------------------------------------

data Env = Env
  { _trc         :: Bool
  , _verbose     :: Bool
  , _journal     :: Bool
  , _stdErrOn    :: Bool
  , _dryRun      :: Bool
  , _forceMDU    :: Bool  -- Meta Data Update
  , _port        :: Int
  , _jsonArchive :: FilePath
  , _jsonImport  :: Maybe FilePath
  , _mountPath   :: FilePath
  , _syncDir     :: FilePath
  , _fontName    :: Text
  , _logOp       :: Maybe (String -> IO ())
  , _updateCache :: Maybe FilePath
  }

-- deriving instance Show Env

instance Config Env where
  traceOn   e = e ^. envTrc
  verboseOn e = e ^. envVerbose
  stderrOn  e = e ^. envStdErrOn

type CopyGeo = ((Int, Int), AspectRatio)

defaultEnv :: Env
defaultEnv = Env
  { _trc          = False
  , _verbose      = False
  , _journal      = False
  , _stdErrOn     = True
  , _dryRun       = False
  , _forceMDU     = False
  , _port         = 3001
  , _jsonArchive  = "catalog.json" -- rel to mount path
  , _jsonImport   = Nothing
  , _mountPath    = "."
  , _syncDir      = s'photos       -- the top archive dir
  , _fontName     = mempty
  , _logOp        = Just (hPutStrLn stderr)
  , _updateCache  = Nothing
  }

envTrc :: Lens' Env Bool
envTrc k e = (\ new -> e {_trc = new}) <$> k (_trc e)

envVerbose :: Lens' Env Bool
envVerbose k e = (\ new -> e {_verbose = new}) <$> k (_verbose e)

envJournal :: Lens' Env Bool
envJournal k e = (\ new -> e {_journal = new}) <$> k (_journal e)

envStdErrOn :: Lens' Env Bool
envStdErrOn k e = (\ new -> e {_stdErrOn = new}) <$> k (_stdErrOn e)

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

envMountPath :: Lens' Env FilePath
envMountPath k e = (\ new -> e {_mountPath = new}) <$> k (_mountPath e)

envSyncDir :: Lens' Env FilePath
envSyncDir k e = (\ new -> e {_syncDir = new}) <$> k (_syncDir e)

envFontName :: Lens' Env Text
envFontName k e = (\ new -> e {_fontName = new}) <$> k (_fontName e)

envLogOp :: Lens' Env (Maybe (String -> IO ()))
envLogOp k e = (\ new -> e {_logOp = new}) <$> k (_logOp e)

envUpdateCache :: Lens' Env (Maybe FilePath)
envUpdateCache k e = (\ new -> e {_updateCache = new}) <$> k (_updateCache e)

-- ----------------------------------------

type Cmd = Action Env ImgStore

runCmd :: Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd = runCmd' defaultEnv

runCmd' :: Env -> Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd' env cmd = do
  logC <- logCmd           -- set the syncronized write to stderr as log cmd
  let env' = env & envLogOp .~ Just logC
  runAction cmd env' emptyImgStore

-- synchronize the access to stderr
--
-- this syncronizes log messages from different threads,
-- but does not work for server logging and application log messages
logCmd :: IO (String -> IO ())
logCmd = do
  sem <- newQSem 0
  return $ \ s ->
    bracket_ (waitQSem sem) (signalQSem sem)
    ( do hPutStrLn stderr s
         hFlush    stderr
    )

-- ----------------------------------------
