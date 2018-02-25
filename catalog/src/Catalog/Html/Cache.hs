{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- TODO: traversal over all geometies and all collections not yet ready

module Catalog.Html.Cache
where

import Catalog.Cmd
import Catalog.Html.Basic
import Catalog.System.Convert (genImageFrom)

import Data.ImgTree
import Data.Prim

fillImgCache :: Path -> Cmd ()
fillImgCache =
  fillImgCacheP'
  (\ f -> do
      let fs = map (fg f)
            [ GeoAR 1920 1200 Pad
            , GeoAR 1400 1050 Pad
            , GeoAR  900  600 Pad
            , GeoAR   160 160 Pad
            , GeoAR   160 120 Fix
            , GeoAR   140 105 Fix
            ]
      mapM_ genImage' fs
  )
  where
    fg f geo = "/" ++ geo ^. isoString ++ f
    genImage' f = (genImage f >> return ()) `catchE` (\ _e -> return ())
    genImage f  = abort "fillCache: not yet implemented"
--    genImage' f = local (\ env -> env & envDryRun  .~ True
--                                      & envVerbose .~ True) (genImage f)

fillImgCacheP' :: (FilePath -> Cmd ()) -> Path -> Cmd ()
fillImgCacheP' cmd p = do
  (i, n) <- getIdNode' p
  unless (isCOL n) $
    abort $ "fillImgCacheP: path does not specify a collection: " ++ quotePath p
  verbose $ "fillImgCacheP: filling cache for: " ++ quotePath p
  fillImgCache' cmd i

fillImgCache' :: (FilePath -> Cmd ()) -> ObjId -> Cmd ()
fillImgCache' cmd =
  foldCollections fill
  where
    fill go i _md im _be es = do
      maybe (return ()) (uncurry updateImg) im
      updateColImg i
      mapM_ (colEntry updateImg go) es

    updateImg i n =
      buildImgPath i n >>= cmd

    updateColImg i =
      colImgRef i >>= cmd
