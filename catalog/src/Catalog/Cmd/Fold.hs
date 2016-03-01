module Catalog.Cmd.Fold
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Control.Lens
import           Control.Lens.Util
import           Data.ImageTree
import           Data.MetaData
import           Data.Prim.PathId
import           Data.Prim.Prelude
import           Data.Prim.TimeStamp
import           Data.RefTree

-- ----------------------------------------

processImages :: Monoid r =>
                 (ObjId -> ImgParts -> Cmd r) -> ObjId -> Cmd r
processImages imgA i0 =
  foldMT imgA dirA rootA colA i0
  where
    dirA  go _i      es _ts = mconcat <$> traverse go (es ^. isoSetList)
    rootA go _i dir _col    = go dir
    colA  _  _i _md _es _ts = return mempty

-- ----------------------------------------

type Act r = ObjId -> Cmd r

foldMT :: (         ObjId -> ImgParts                            -> Cmd r) ->  -- IMG
          (Act r -> ObjId -> Set ObjId              -> TimeStamp -> Cmd r) ->  -- DIR
          (Act r -> ObjId -> ObjId    -> ObjId                   -> Cmd r) ->  -- ROOT
          (Act r -> ObjId -> MetaData -> [ColEntry] -> TimeStamp -> Cmd r) ->  -- COL
           Act r
foldMT imgA dirA' rootA' colA' i0 = do
  go i0
  where
    dirA  = dirA'  go
    rootA = rootA' go
    colA  = colA'  go
    go i  = do
      trcObj i $ "foldMT"
      n <- getTree (theNode i)
      case n ^. nodeVal of
        IMG pts ->
          imgA i pts
        DIR es ts ->
          dirA i es ts
        ROOT dir col ->
          rootA i dir col
        COL md es ts ->
          colA i md es ts

-- ----------------------------------------
