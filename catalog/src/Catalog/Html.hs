{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Html
where

import           Catalog.Cmd
import           Catalog.FilePath
import           Data.ImgTree
import           Data.Prim

-- ----------------------------------------

type Html = ()

type Neighbors a = (Maybe a, Maybe a, Maybe a) -- prev, next, parent

allNeighbors :: Traversal (Neighbors a) (Neighbors b) a b
allNeighbors = each . _Just
{-# INLINE allNeighbors #-}

neighborPaths :: Neighbors ObjId -> Cmd (Neighbors Path)
neighborPaths = traverseOf allNeighbors objid2path

htmlCollection :: Path -> Cmd Html
htmlCollection col'path = do
  (i, val)  <- getIdNode "htmlCollection: collection not found" col'path
  unless (isCOL val) $
    abort $ "htmlCollection: not a collection" ++ show (show col'path)
  htmlCol i


htmlCol :: ObjId -> Cmd Html
htmlCol i = do
  -- the path of the collection
  i'p <- objid2path

  -- the ids of the neighbors, and the pos in parent collection
  -- neded for nav info
  (neighbor'is, _pos) <- getNeighbors i

  -- the paths of the neighbors, needed in href's
  neighbor'ps         <- traverseOf allNeighbors objid2path neighbor'is

  -- the image parts of the neighbors, needed for img refs
  -- and the names of the neighbors
  neighbos'ims        <- traverseOf allNeighbors getImgParts neighbor'is

  -- the meta data collection image and contents
  (COL md mbi cs _ts) <- getImgVal i

  -- the image parts and the names of all images in the collection
  cs'ims              <- traverseOf traverse getColParts cs

  return undefined

getImgParts :: ObjId -> Cmd (ImgParts, Name)
getImgParts = undefined

getColParts :: ColEntry -> Cmd (Path, (ImgParts, Name))
getColParts = undefined

getNeighbors :: ObjId -> Cmd (Neighbors ObjId, Int)
getNeighbors i = do
  i'p <- getImgParent i
  v'p <- getImgVal i'p
  if (not $ isCOL v'p)
    then return ((Nothing, Nothing, Nothing), 0)  -- top collection hasn't a parent coll nor neighbors
    else do
      let ce = v'p ^. theColEntries
      case searchPos (\c' -> c' ^. theColObjId == i) ce of
        Just 0 ->
          return ((Nothing, ref1 $ tail ce, Just i'p), 0)
        Just i' ->
          let ce1 = drop (i' - 1) ce
              r1  = ref1 ce1
              r2  = ref1 (drop 2 ce1)
          in return ((r1, r2, Just i'p), i')
        Nothing -> do
          p <- objid2path i
          abort $ "getNeighbors: ObjId not found in parent collection" ++ show (show p)
  where
    ref1 ce' = (^. theColObjId) <$> listToMaybe ce'
