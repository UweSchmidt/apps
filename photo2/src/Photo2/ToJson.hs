{-# LANGUAGE OverloadedStrings #-}

module Photo2.ToJson
       ( colToJson
       )
where

import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.Map as M
import Data.List (isPrefixOf)

import Photo2.ArchiveTypes
import Photo2.Arrow

import Data.Tree.NTree.TypeDefs
import System.FilePath

import Text.XML.HXT.Core

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB

colToJson :: FilePath -> PathArrow AlbumTree ()
colToJson file path =
  getTree path
  >>>
  arr (treeToJson path)
  >>>
  arrIO (outJson file)

treeToJson :: Path -> AlbumTree -> J.Value
treeToJson p (NTree pic cs) =
  J.object [ "path" J..= (T.pack $ p')
           , "type" J..= (T.pack $ if isAl pic then "ALBUM" else "PIC")
           , "jpg"  J..= (T.pack $ picOrig pic)
           , "raw"  J..= (T.pack $ picRaw  pic)
           , "meta" J..= attrsToJson (picAttrs pic)
           , "cont" J..= contToJson (p ++ [picId pic]) cs
           ]
  where
    p' = foldr (</>) (picId pic) p

attrsToJson :: Attrs -> J.Value
attrsToJson am = J.object $ concatMap toJ $ M.toList am
  where
    toJ (a, v)
      | isImp     = [T.pack a' J..= T.pack v]
      | otherwise = []
      where
        a' = show a
        isImp =
          "descr:" `isPrefixOf` a'
          ||
          "show:" `isPrefixOf` a'

contToJson :: Path -> [AlbumTree] -> J.Value
contToJson p cs = J.toJSON $ map (treeToJson p) cs

outJson :: FilePath -> J.Value -> IO ()
outJson f v = do
  LB.writeFile f (J.encodePretty v)
