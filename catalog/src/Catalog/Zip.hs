{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Zip
where

import Catalog.Cmd
import Catalog.System.ExifTool (syncMetaData)
import Data.ImgTree
import Data.Prim

-- ----------------------------------------

zipCollection' :: Path -> Cmd FilePath
zipCollection' p = do
  verbose $ "zipCollection: " ++ quotePath p
  mbi <- lookupByPath p
  maybe
    (abort $ "zipCollection: illegal path " ++ p ^. isoString)
    (uncurry zipCollection) mbi

zipCollection :: ObjId -> ImgNode -> Cmd FilePath
zipCollection i e
  | isCOL e = do
      trcObj i "zipColl: create zip archive"
      p <- objid2path i
      f <- toFilePath p
      let abf = ps'zipcache <> p ^. isoString
      let azf = abf <> ".zip"
      trcObj i $ "zipColl: create zip archive " ++ azf ++ " for collection " ++ show p
      return $ ps'zipcache </> "test.zip"
      -- a lot to do
      -- return azf

  | otherwise = do
      trcObj i "zipCollection: not a collection "
      abort $  "zipCollection: not a collection"


-- ----------------------------------------
