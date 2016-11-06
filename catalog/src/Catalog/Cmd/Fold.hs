module Catalog.Cmd.Fold
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------

type Act r = ObjId -> Cmd r

foldMT :: (         ObjId -> ImgParts -> MetaData                -> Cmd r) ->  -- IMG
          (Act r -> ObjId -> DirEntries             -> TimeStamp -> Cmd r) ->  -- DIR
          (Act r -> ObjId -> ObjId    -> ObjId                   -> Cmd r) ->  -- ROOT
          (Act r -> ObjId -> MetaData -> (Maybe (ObjId, Name))
                                      -> (Maybe (ObjId, Name))
                                      -> [ColEntry] -> TimeStamp -> Cmd r) ->  -- COL
           Act r
foldMT imgA dirA' rootA' colA' i0 = do
  go i0
  where
    dirA  = dirA'  go
    rootA = rootA' go
    colA  = colA'  go
    go i  = do
      -- trcObj i $ "foldMT"
      n <- getTree (theNode i)
      -- trc $ "foldMT: " ++ show n
      case n ^. nodeVal of
        IMG pts md ->
          imgA i pts md
        DIR es ts ->
          dirA i es ts
        ROOT dir col ->
          rootA i dir col
        COL md im be es ts ->
          colA i md im be es ts

-- ----------------------------------------

processImgDirs :: Monoid r =>
                  (         ObjId -> ImgParts   -> MetaData  -> Cmd r) ->
                  (Act r -> ObjId -> DirEntries -> TimeStamp -> Cmd r) ->
                  Act r
processImgDirs imgA dirA =
  foldMT imgA dirA rootA colA
  where
    rootA go _i dir _col            = go dir
    colA  _  _i _md _im _be _es _ts = return mempty

-- ----------------------------------------

processCollections ::
  Monoid r =>
  (Act r -> ObjId -> MetaData -> (Maybe (ObjId, Name))
                              -> (Maybe (ObjId, Name))
                  -> [ColEntry] -> TimeStamp -> Cmd r) ->
  Act r

processCollections colA =
  foldMT imgA dirA rootA colA
  where
    rootA go _i dir _col  = go dir
    dirA  _  _  _es _ts   = return mempty
    imgA  _  _pts   _md   = return mempty

-- ----------------------------------------

processImages :: Monoid r =>
                 (ObjId -> ImgParts -> MetaData -> Cmd r) -> ObjId -> Cmd r
processImages imgA =
  processImgDirs imgA dirA
  where
    dirA  go _i es _ts = mconcat <$> traverse go (es ^. isoDirEntries)

-- ----------------------------------------
