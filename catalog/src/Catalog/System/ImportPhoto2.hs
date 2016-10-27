{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.ImportPhoto2
where

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
import qualified Data.Aeson as J
import           Data.Map (Map)
import qualified Data.Map as M

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
    ppx = p'albums
    go (IC pt0 ty jpg raw (MD2 md) cont) = do
      -- verbose $ "iip2: " ++ show path
      case ty of
        PIC   -> insPic
        ALBUM -> insAlbum
      -- lists are pprocesed from the end
      -- so the result lists can be constructed by consing, not appending elements
      mapM_ go $ reverse cont
        where
          path :: Path
          path = ("/" <> pt0) ^. isoString . from isoString . to (tailPath . concPath ppx)
          (dp, nm) = path ^. viewBase

          insPic = do
            verbose $ unwords ["PIC ", show pno, show dp]
            return ()
              where
              pno :: Int
              pno = read $ drop 4 $ show nm

          insAlbum = do
            verbose $ "ALBUM: " ++ show path

ttt = runCmd $ local ( & envVerbose %~ (const True)) $ (loadImportData "data/export.json") >>= insertImportPhoto2 >> return ()
