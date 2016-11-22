module Catalog.Cmd.Fold
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------

type Act r = ObjId -> Cmd r

foldMT' :: (         ObjId                                        -> Cmd r) ->  -- undef id
           (         ObjId -> ImgParts -> MetaData                -> Cmd r) ->  -- IMG
           (Act r -> ObjId -> DirEntries             -> TimeStamp -> Cmd r) ->  -- DIR
           (Act r -> ObjId -> ObjId    -> ObjId                   -> Cmd r) ->  -- ROOT
           (Act r -> ObjId -> MetaData -> (Maybe (ObjId, Name))
                           -> (Maybe (ObjId, Name)) -> [ColEntry] -> Cmd r) ->  -- COL
           Act r
foldMT' undefId imgA dirA' rootA' colA' i0 = do
  go i0
  where
    dirA  = dirA'  go
    rootA = rootA' go
    colA  = colA'  go
    go i  = do
      -- trcObj i $ "foldMT"
      mn <- getTree (entryAt i)
      -- trc $ "foldMT: " ++ show mn
      case mn of
        Nothing ->
          undefId i
        Just n ->
          case n ^. nodeVal of
            IMG pts md ->
              imgA i pts md
            DIR es ts ->
              dirA i es ts
            ROOT dir col ->
              rootA i dir col
            COL md im be es ->
              colA i md im be es

foldMT :: (         ObjId -> ImgParts -> MetaData                -> Cmd r) ->  -- IMG
          (Act r -> ObjId -> DirEntries             -> TimeStamp -> Cmd r) ->  -- DIR
          (Act r -> ObjId -> ObjId    -> ObjId                   -> Cmd r) ->  -- ROOT
          (Act r -> ObjId -> MetaData -> (Maybe (ObjId, Name))
                          -> (Maybe (ObjId, Name)) -> [ColEntry] -> Cmd r) ->  -- COL
           Act r
foldMT = foldMT' undefId
  where
    undefId i = do
      warn  $ "foldMT: undefined obj id found: " ++ show i
      abort $ "foldMT: undefined obj id found: " ++ show i

-- ----------------------------------------

foldImgDirs :: Monoid r =>
               (         ObjId -> ImgParts   -> MetaData  -> Cmd r) ->
               (Act r -> ObjId -> DirEntries -> TimeStamp -> Cmd r) ->
               Act r
foldImgDirs imgA dirA =
  foldMT imgA dirA rootA colA
  where
    rootA go _i dir _col        = go dir
    colA  _  _i _md _im _be _es = return mempty

-- ----------------------------------------

foldCollections ::
  Monoid r =>
  (Act r -> ObjId -> MetaData -> (Maybe (ObjId, Name))
                  -> (Maybe (ObjId, Name)) -> [ColEntry] -> Cmd r) ->
  Act r

foldCollections colA =
  foldMT imgA dirA rootA colA
  where
    rootA go _i dir _col  = go dir
    dirA  _  _  _es _ts   = return mempty
    imgA  _  _pts   _md   = return mempty

-- ----------------------------------------

foldImages :: Monoid r =>
              (ObjId -> ImgParts -> MetaData -> Cmd r) -> ObjId -> Cmd r
foldImages imgA =
  foldImgDirs imgA dirA
  where
    dirA  go _i es _ts = mconcat <$> traverse go (es ^. isoDirEntries)

-- ----------------------------------------
