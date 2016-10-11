{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Cmd.Types
       ( module Catalog.Cmd.Types
       , module Control.Monad.RWSErrorIO
       , module Control.Monad.Except
       )
where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.Prim

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
  , _mountPath   :: FilePath
  , _fontName    :: Text
  }

deriving instance Show Env

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
  , _mountPath    = "."
  , _fontName     = mempty
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

envMountPath :: Lens' Env FilePath
envMountPath k e = (\ new -> e {_mountPath = new}) <$> k (_mountPath e)

envFontName :: Lens' Env Text
envFontName k e = (\ new -> e {_fontName = new}) <$> k (_fontName e)

-- ----------------------------------------

type Cmd = Action Env ImgStore

runCmd :: Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd = runCmd' defaultEnv

runCmd' :: Env -> Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd' env cmd = runAction cmd env emptyImgStore

-- ----------------------------------------
