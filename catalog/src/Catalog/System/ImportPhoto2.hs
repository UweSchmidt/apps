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
import           System.FilePath (dropExtension, (</>))
import           Data.Map (Map)
import qualified Data.Map  as M
import qualified Data.Text as T
import           Text.Read(readMaybe)

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

-- throw away last import
cleanImportCols :: Cmd ()
cleanImportCols = do
  lookupByPath p'imports >>= maybe (return ()) (rmRec . fst)
  genImportsCollection

insertImportPhoto2 :: ImportCol -> Cmd ()
insertImportPhoto2 ipd = go ipd
  where
    ppx = p'imports
    ppi = p'arch'photos
    go (IC pt0 ty jpg raw (MD2 md) cont) = do
      -- verbose $ "iip2: " ++ show path
      ( case ty of
          PIC   ->
            insPic
          ALBUM ->
            insAlbum
            >>
            -- lists are processed from the end
            -- so the result lists can be constructed by consing, not appending elements
            mapM_ go (reverse cont)
        )
        `catchE`
        (\ e -> warn $ "iip2: can't import image/collection: " ++ show e)
        where
          toPath :: Text -> Path
          toPath s = ("/" <> s) ^. isoString . from isoString

          toArch :: Path -> Path
          toArch = (ppi `concPath`)

          imgPath :: Path
          imgName :: Name
          (imgPath, imgName)
            | T.null raw =
                (toArch jpgPath', jpgPart)
            | otherwise =
                (toArch rawPath,  jpgP)
            where
              (rawPath',
               rawName)   = toPath raw ^. viewBase
              rawName'    = rawName & isoString %~ dropExtension
              rawPath     = rawPath' `snocPath` rawName'

              jpgN        = remCommonPathPrefix (toPath raw) (toPath jpg) ^. _2
              jpgP        = jpgN ^. isoString . to (drop 1) . from isoString
              (jpgPath,
               jpgName)   = toPath jpg ^. viewBase
              (jpgDir,
               jpgBase)   = jpgPath    ^. viewBase
              name        = jpgName & isoString %~ dropExtension
              jpgBaseName = jpgBase ^. isoString </> jpgName ^. isoString
              jpgType     = filePathToImgType jpgBaseName ^. _2
              jpgPath'
                | jpgType == IMGother = jpgPath `snocPath` name
                | otherwise           = jpgDir  `snocPath` name
              jpgPart
                | jpgType == IMGother = jpgName
                | otherwise           = jpgBaseName ^. from isoString

          path           = concPath ppx . tailPath . toPath $ pt0
          (colPath, cnm) = path ^. viewBase

          insPic = do
            verbose $
              unwords ["PIC ", show pno, show colPath, show imgPath, show imgName]
            cin <- lookupByPath colPath
            iin <- lookupByPath imgPath
            flip (maybe (verbose $ "img not found: " ++ show imgPath)) iin $
              \ (iid, _iin) ->
                flip (maybe (verbose $ "col not found: " ++ show colPath)) cin $
                  \ (cid, _cin) -> do
                    adjustColEntries (mkColImgRef iid imgName :) cid
            where
              pno :: Maybe Int
              pno = readMaybe $ drop 4 $ show cnm

          insAlbum = do
            verbose $ unwords ["ALBUM ", show path, show imgPath, show imgName]
            cin <- lookupByPath path

            -- create the collection and/or get the collection id
            cid <- maybe (mkCollectionC path) (return . fst) cin

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
      loadImgStore "catalog/photos.pathid.json" >>
      genSysCollections >>
      cleanImportCols >>
      (loadImportData "export.json") >>=
      insertImportPhoto2 >>
      return ()
