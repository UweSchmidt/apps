{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.ImportPhoto2
where

import           Catalog.Cmd
import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Catalog.System.IO
import           Catalog.FilePath
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import           Data.ImageStore
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim
import qualified System.FilePath as FP
import           Data.Map (Map)
import qualified Data.Map  as M
import qualified Data.Text as T

-- ----------------------------------------

data AlbumOrPic = ALBUM | PIC
                deriving (Eq, Ord, Show, Read)

data ImportCol' path = IC
  { _path :: path
  , _type :: AlbumOrPic
  , _jpg  :: Text
  , _raw  :: Text
  , _meta :: MD2
  , _cont :: [ImportCol' path]
  }
  deriving (Show, Functor)

type ImportCol = ImportCol' Text

newtype MD2 = MD2 MetaData
            deriving Show

instance J.FromJSON AlbumOrPic where
  parseJSON = J.withText "AlbumOrPic" $ \ t ->
    case t of
      "ALBUM" -> return ALBUM
      "PIC"   -> return PIC
      _       -> mzero

instance J.FromJSON MD2 where
  parseJSON = J.withObject "MD2" $
    return . MD2 . MD

instance (J.FromJSON path) => J.FromJSON (ImportCol' path) where
  parseJSON = J.withObject "ImportCol" $ \ o ->
    IC <$> o J..: "path"
       <*> o J..: "type"
       <*> o J..: "jpg"
       <*> o J..: "raw"
       <*> o J..: "meta"
       <*> o J..: "cont"

loadImportData :: FilePath -> Cmd ImportCol
loadImportData p = do
  bs <- readFileLB p
  case fromBS bs of
    Nothing ->
      abort $ "loadImportData: JSON input corrupted: " ++ show p
    Just ipd ->
      return ipd
  where
    fromBS = J.decode'

insertImportPhoto2 :: ImportCol -> Cmd ()
insertImportPhoto2 ipd = go ipd
  where
    ppx = p'imports
    ppi = p'arch'photos
    go (IC pt0 ty jpg raw (MD2 md) cont) = do
      -- verbose $ "iip2: " ++ show path
      case ty of
        PIC   -> insPic
        ALBUM -> insAlbum
      -- lists are processed from the end
      -- so the result lists can be constructed by consing, not appending elements
      mapM_ go $ reverse cont
        where
          toPath :: Text -> Path
          toPath s = ("/" <> s) ^. isoString . from isoString

          path           = concPath ppx . tailPath . toPath $ pt0
          (colPath, cnm) = path ^. viewBase

          imgPath0
            | T.null raw  = toPath jpg
            | otherwise   = toPath raw

          imgPath = ppi `concPath` imgPath1

          (imgPath1, imgName)
            | ty21 /= IMGother = (p2 `snocPath` nm21, n21)
            | otherwise        = (p1 `snocPath` nm1 , n1 )
            where
              (p1, n1)     = imgPath0 ^. viewBase
              (p2, n2)     = p1       ^. viewBase
              n21          = mkName $ (n2 ^. isoString) ++ "/" ++ (n1 ^. isoString)
              (nm21, ty21) = n21      ^. isoString . to filePathToImgType
              (nm1,  ty1 ) = n1       ^. isoString . to filePathToImgType

          insPic = do
            verbose $
              unwords ["PIC ", show pno, show path, show imgPath, show imgName]
            cin <- lookupByPath path
            iin <- lookupByPath imgPath
            flip (maybe (verbose $ "img not found: " ++ show imgPath)) iin $
              \ (iid, _iin) ->
                flip (maybe (verbose $ "col not found: " ++ show path)) cin $
                  \ (cid, _cin) -> do
                    adjustColEntries (mkColImgRef iid imgName :) cid
            where
              pno :: Int
              pno = read $ drop 4 $ show cnm

          insAlbum = do
            verbose $ unwords ["ALBUM ", show colPath, show imgPath, show imgName]
            cin <- lookupByPath colPath

            -- create the collection and/or get the collection id
            cid <- maybe (mkCollectionC colPath) (return . fst) cin

            -- set the meta data and the collection image
            adjustMetaData (md <>) cid
            iin <- fst <$> getIdNode' imgPath
            adjustColImg (const $ Just (iin, imgName)) cid

-- run in data subdir
ttt = runCmd $
      local (& envVerbose .~ True) $
      (do
          mp' <- view envMountPath
          jp' <- view envJsonArchive
          initImgStore n'archive n'collections
            (mp' </> s'photos)) >>
      genSysCollections >>
      (loadImportData "export.json") >>=
      insertImportPhoto2 >>
      return ()
