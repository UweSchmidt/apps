{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Catalog.Workflow
where

import Data.Prim
import Data.ImgNode
import Data.ImgTree
import Catalog.Cmd
import qualified Text.Blaze.Html      as Blaze


data Req
  = Req { _reqPath  :: Path
        , _reqIx    :: Maybe Int
        , _reqGeoAR :: GeoAR
        , _reqColId :: ObjId
        , _reqColNd :: ImgNode
        , _reqImgId :: ObjId
        , _reqImgNd :: ImgNode
        , _reqIName :: Name
        , _reqIType :: ImgType
        , _reqRes   :: LazyByteString
        }

deriving instance Show Req

emptyReq :: Req
emptyReq =
  Req { _reqPath  = mempty
      , _reqIx    = Nothing
      , _reqGeoAR = geoar'org
      , _reqColId = mempty
      , _reqColNd = emptyImgCol
      , _reqImgId = mempty
      , _reqImgNd = emptyImg
      , _reqIName = mempty
      , _reqIType = mempty
      , _reqRes   = mempty
      }

-- --------------------

reqPath :: Lens' Req Path
reqPath k r = (\ new -> r {_reqPath = new}) <$> k (_reqPath r)

reqIx :: Lens' Req (Maybe Int)
reqIx k r = (\ new -> r {_reqIx = new}) <$> k (_reqIx r)

reqGeoAR :: Lens' Req GeoAR
reqGeoAR k r = (\ new -> r {_reqGeoAR = new}) <$> k (_reqGeoAR r)

reqColId :: Lens' Req ObjId
reqColId k r = (\ new -> r {_reqColId = new}) <$> k (_reqColId r)

reqColNd :: Lens' Req ImgNode
reqColNd k r = (\ new -> r {_reqColNd = new}) <$> k (_reqColNd r)

reqImgId :: Lens' Req ObjId
reqImgId k r = (\ new -> r {_reqImgId = new}) <$> k (_reqImgId r)

reqImgNd :: Lens' Req ImgNode
reqImgNd k r = (\ new -> r {_reqImgNd = new}) <$> k (_reqImgNd r)

reqIName :: Lens' Req Name
reqIName k r = (\ new -> r {_reqIName = new}) <$> k (_reqIName r)

reqIType :: Lens' Req ImgType
reqIType k r = (\ new -> r {_reqIType = new}) <$> k (_reqIType r)

reqRes :: Lens' Req LazyByteString
reqRes k r = (\ new -> r {_reqRes = new}) <$> k (_reqRes r)

-- ----------------------------------------
--
-- dispatch HTML page generation
-- depending on type of object (collection, image, blog entry)

processHtmlPage :: Req -> Cmd Req
processHtmlPage r = do
  (i, n) <- getIdNode' (r ^. reqPath)
  unless (isCOL n) $
    abortWF "processHtmlPage: path not found: " r

  -- set collection id and collection node
  let r' = r & reqColId .~ i
             & reqColNd .~ n

  case r ^. reqIx of
    Just ix' -> do
      ce <- colEntryAt ix' n
      colEntry
        (\ i' nm' ->
            -- set image id and image name
            -- and process image (or blog entry or ...)
            processHtmlObjPage (r' & reqImgId .~ i'
                                   & reqIName .~ nm'
                               )
        )
        (\ c' -> do
            -- normalize path to a collection (remove ix)
            -- and restart processing
            p' <- objid2path c'
            processHtmlPage    (r' & reqPath .~ p'
                                   & reqIx   .~ Nothing
                               )
        )
        ce
    Nothing ->
      -- generate a collection page
      processHtmlColPage r'


processHtmlObjPage :: Req -> Cmd Req
processHtmlObjPage r = do
  n  <- getImgVal (r ^. reqImgId)

  let r' = r & reqImgNd .~ n

  -- compute image type
  ty <- maybe
        (abortWF "processHtmlObjPage: image part not found" r')
        return
        (n ^? theImgPart (r' ^. reqIName) . theImgType)

  -- and store type
  let r'' = r' & reqIType .~ ty

  -- dispatch over image type
  case ty of
    IMGtxt
        -> processHtmlBlogPage r''
    _
      | ty `elem` [IMGjpg, IMGimg, IMGcopy]
        -> processHtmlImgPage r''

      | otherwise
        -> abortWF "processHtmlObjPage: HTML for image type not supported" r''

processHtmlColPage :: Req -> Cmd Req
processHtmlColPage r = undefined

processHtmlImgPage :: Req -> Cmd Req
processHtmlImgPage r = undefined

processHtmlBlogPage :: Req -> Cmd Req
processHtmlBlogPage r = undefined

abortWF :: String -> Req -> Cmd a
abortWF msg r = abort (msg ++ ": req = " ++ show r)

-- ----------------------------------------
