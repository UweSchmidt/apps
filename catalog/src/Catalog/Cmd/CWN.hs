module Catalog.Cmd.CWN
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.List
import           Catalog.Cmd.Types
import           Control.Lens
import           Data.ImageStore
import           Data.ImgTree
import           Data.Prim

-- ----------------------------------------
--
-- ops on current node


we :: Cmd ObjId
we = use theWE

withCWN :: (ObjId -> ImgTree -> Cmd a) -> Cmd a
withCWN cmd
  = do wd <- we
       t  <- dt
       cmd wd t

-- change working node
cwSet :: ObjId -> Cmd ()
cwSet i = do
  e <- getTree (entryAt i)
  case e of
    Nothing ->
      abort $ "cwSet: node not found: " ++ show i
    Just _ ->
      theWE .= i

cwSetPath :: Path -> Cmd ()
cwSetPath p =
  cwSet (mkObjId p)
  `catchError`
  (\ _e -> abort $ "cwSetPath: no such node " ++ show p)

-- | change working node to root node
cwRoot :: Cmd ()
cwRoot = getTree rootRef >>= cwSet

-- | change working node to parent

cwUp :: Cmd ()
cwUp = do
  ip <- we >>= getImgParent
  theWE .= ip


cwDown :: Name -> Cmd ()
cwDown d = do
  p <- flip snocPath d <$> cwPath
  cwSetPath p

cwType :: Cmd String
cwType = we >>= objid2type

cwPath :: Cmd Path
cwPath = we >>= objid2path

-- | list names of elements in current node
cwLs :: Cmd [Name]
cwLs = we >>= objid2contNames

-- | convert working node path to file system path
-- cwFilePath :: Cmd SysPath
-- cwFilePath = cwPath >>= path2SysPath

cwListPaths :: Cmd String
cwListPaths = we >>= listPaths

cwListNames :: Cmd String
cwListNames = we >>= listNames

-- ----------------------------------------
