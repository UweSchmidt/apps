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
  { _copyGeo     :: [GeoAR]
  , _metaSrc     :: [ImgType]
  , _trc         :: Bool
  , _verbose     :: Bool
  , _dryRun      :: Bool
  , _port        :: Int
  , _jsonArchive :: FilePath
  , _mountPath   :: FilePath
  }

deriving instance Show Env

instance Config Env where
  traceOn   e = e ^. envTrc
  verboseOn e = e ^. envVerbose

type CopyGeo = ((Int, Int), AspectRatio)

defaultEnv :: Env
defaultEnv = Env
  { _copyGeo     = [ GeoAR 1400 1050 Pad
                   , GeoAR  160  160 Pad
                   , GeoAR  160  120 Fix
                   ]
  , _metaSrc     = [IMGraw, IMGimg, IMGmeta]
  , _trc         = True
  , _verbose     = True
  , _dryRun      = False
  , _port        = 3001
  , _jsonArchive = "catalog.json" -- rel to mount path
  , _mountPath   = "./data"
  }

envCopyGeo :: Lens' Env [GeoAR]
envCopyGeo k e = (\ new -> e {_copyGeo = new}) <$> k (_copyGeo e)

envMetaSrc :: Lens' Env [ImgType]
envMetaSrc k e = (\ new -> e {_metaSrc = new}) <$> k (_metaSrc e)

envTrc :: Lens' Env Bool
envTrc k e = (\ new -> e {_trc = new}) <$> k (_trc e)

envVerbose :: Lens' Env Bool
envVerbose k e = (\ new -> e {_verbose = new}) <$> k (_verbose e)

envDryRun :: Lens' Env Bool
envDryRun k e = (\ new -> e {_dryRun = new}) <$> k (_dryRun e)

envPort :: Lens' Env Int
envPort k e = (\ new -> e {_port = new}) <$> k (_port e)

envJsonArchive :: Lens' Env FilePath
envJsonArchive k e = (\ new -> e {_jsonArchive = new}) <$> k (_jsonArchive e)

envMountPath :: Lens' Env FilePath
envMountPath k e = (\ new -> e {_mountPath = new}) <$> k (_mountPath e)

-- ----------------------------------------

type Cmd = Action Env ImgStore

runCmd :: Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd cmd = runAction cmd defaultEnv emptyImgStore

runCmd' :: Env -> Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd' env cmd = runAction cmd env emptyImgStore

-- ----------------------------------------
