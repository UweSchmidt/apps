{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Cmd.Types
       ( module Catalog.Cmd.Types
       , module Control.Monad.RWSErrorIO
       , module Control.Monad.Except
       )
where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.ImgAction
import           Data.Prim.ImageType

-- ----------------------------------------

data Env = Env
  { _copyGeo :: [CopyGeo]
  , _metaSrc :: [ImgType]
  , _trc     :: Bool
  , _verbose :: Bool
  , _dryRun  :: Bool
  }

deriving instance Show Env

instance Config Env where
  traceOn   e = e ^. envTrc
  verboseOn e = e ^. envVerbose

type CopyGeo = ((Int, Int), AspectRatio)

initEnv :: Env
initEnv = Env
  { _copyGeo = [ ((1400, 1050), Pad)
               , (( 160,  160), Pad)
               , (( 160,  120), Fix)
               ]
  , _metaSrc = [IMGraw, IMGimg, IMGmeta]
  , _trc     = True
  , _verbose = True
  , _dryRun  = False
  }

envCopyGeo :: Lens' Env [CopyGeo]
envCopyGeo k e = (\ new -> e {_copyGeo = new}) <$> k (_copyGeo e)

envMetaSrc :: Lens' Env [ImgType]
envMetaSrc k e = (\ new -> e {_metaSrc = new}) <$> k (_metaSrc e)

envTrc :: Lens' Env Bool
envTrc k e = (\ new -> e {_trc = new}) <$> k (_trc e)

envVerbose :: Lens' Env Bool
envVerbose k e = (\ new -> e {_verbose = new}) <$> k (_verbose e)

envDryRun :: Lens' Env Bool
envDryRun k e = (\ new -> e {_dryRun = new}) <$> k (_dryRun e)

-- ----------------------------------------

type Cmd = Action Env ImgStore

runCmd :: Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd cmd = runAction cmd initEnv emptyImgStore

-- ----------------------------------------
