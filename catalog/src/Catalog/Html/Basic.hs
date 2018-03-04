{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Catalog.Html.Basic
where

import Data.ImgTree
import Data.MetaData
import Data.Prim

import Catalog.Cmd
import Catalog.FilePath       ( addJpg )
import Catalog.Journal        ( Journal'(SaveBlogText) )
import Catalog.System.Convert ( genAssetIcon
                              , genBlogText
                              , genBlogHtml
                              , writeBlogText
                              )

import Text.SimpleParser

-- ----------------------------------------

type ColRef' a   = (a, Maybe Int)
type ColRef      = ColRef' ObjId
type ColRefPath  = ColRef' Path

-- ----------------------------------------

maybeColRef :: (ObjId -> a) -> (ObjId -> Int -> a) -> ColRef -> a
maybeColRef cref iref (i, p) =
  case p of
    Nothing  -> cref i
    Just pos -> iref i pos

mkColRefC :: ObjId -> ColRef
mkColRefC i = (i, Nothing)

mkColRefI :: ObjId -> Int -> ColRef
mkColRefI i pos = (i, Just pos)

colImgOp :: Monoid a =>
            (ObjId -> Name -> Cmd a) ->
            (ObjId ->         Cmd a) ->
            ColRef -> Cmd a
colImgOp iop cop = maybeColRef cop iref
  where
    iref i pos = do
      cs <- getImgVals i theColEntries
      case cs ^? ix pos of
        Just (ImgEnt (ImgRef j n)) -> iop j n
        _ -> return mempty

-- ----------------------------------------

colImgType :: ColRef -> Cmd ImgType
colImgType = colImgOp iop cop
  where
    iop i _n = do
      ity <$> getImgVals i theParts
      where
        ity ps
          | thePartNames' (`elem` [IMGimg, IMGjpg]) `has` ps = IMGjpg
          | thePartNames' (== IMGtxt)               `has` ps = IMGtxt
          | otherwise                                        = IMGother
    cop _i = return IMGjpg

colImgName :: ColRef -> Cmd (Maybe Name)
colImgName =
  colImgOp
  (\ _i n -> return $ Just n)
  (\ _i   -> return Nothing)

colImgPath0 :: ColRef -> Cmd (Maybe FilePath)
colImgPath0 = colImgOp iop cop
  where
    cop i = do -- col ref
      n <- getImgVal i
      case n ^? theColImg . traverse of
        -- collection has a front page image
        Just ir ->
          Just <$> buildImgPath0 ir
        _ ->
          return Nothing

    iop j n = Just <$> buildImgPath0 (ImgRef j n)

colImgPath :: ColRef -> Cmd (Maybe FilePath)
colImgPath cr = do
  f <- colImgPath0 cr
  return (addJpg <$> f)

-- compute the image ref of a collection
-- if collection has a front page image, take that
-- otherwise take the ref to a generated image

colImgRef :: ObjId -> Cmd FilePath
colImgRef i = do
  p <- colImgPath (i, Nothing)
  maybe (iconRef i) return p

-- ----------------------------------------

-- compute the icon ref of a collection

iconRef :: ObjId -> Cmd FilePath
iconRef i = do
  t  <- getImgVals i (theMetaData . metaDataAt descrTitle . isoString)

  mbf <-
    if isempty t  -- no title there
    then do
      p <- (^. isoString) <$> objid2path i
      path2img p
    else
      genAssetIcon (t'hash t) t

  return $
    fromMaybe (ps'blank ^. isoString) mbf
  where
    t'hash t' =
      mkCheckSum t' ^. isoString . to toP
      where
        toP s = x ++ "/" ++ y
          where
            (x, y) = splitAt 2 s

-- ----------------------------------------
-- test: is picture a panorama

isPanorama' :: Geo -> Bool
isPanorama' (Geo w h) =
  w >= 2 * h    -- w / h >= 2.0

isPanorama :: Geo -> Geo -> Maybe GeoAR
isPanorama (Geo _w' h') img@(Geo w h)
  | h >= h'        -- height of original >= height of destination image
    &&
    isPanorama' img = Just $ mkGeoAR g Pad
  | otherwise       = Nothing
  where
    g  = Geo w2 h'
    w1 = (h' * w + h' - 1) `div` h
    w2 = (w1 + h' - 1) `div` h' * h'

-- ----------------------------------------

path2img :: FilePath -> Cmd (Maybe FilePath)
path2img f
  | Just (y, Nothing) <- ymd =
      genAssetIcon y y

  | Just (y, Just (m, Nothing)) <- ymd =
      let s = toN m ++ "." ++ y
      in
        genAssetIcon (y </> s) s

  | Just (y, Just (m, Just d)) <- ymd =
      let s = toN d ++ "." ++ toN m ++ "." ++ y
      in
        genAssetIcon (y </> s) s

  | Just n <- nm =
      genAssetIcon n n

  | otherwise =
      return Nothing
  where
    ymd = parseMaybe ymdParser f
    nm  = parseMaybe baseNameParser f

    toN :: String -> String
    toN s = show i   -- remove leading 0's
      where
        i :: Int
        i = read s

-- "/archive/collections/byCreateDate/2000/12/24" -> "2000", "12", "24"
-- "/archive/collections/byCreateDate/2000/12"    -> "2000", "12"
-- "/archive/collections/byCreateDate/2000"       -> "2000"

ymdParser :: SP (String, Maybe (String, Maybe String))
ymdParser = do
  y  <- string ps'bycreatedate *>
        char '/' *>
        count 4 digitChar
  md <- optional $ do
        m <- char '/' *>
             count 2 digitChar
        d <- optional $ char '/' *>
                        count 2 digitChar
        return (m, d)
  return (y, md)

-- "/archive/collections/photos" -> "photos"
baseNameParser :: SP String
baseNameParser =
  char '/' *> many (try $ anyStringThen' (char '/')) *> some anyChar

-- ----------------------------------------

colBlogCont :: ImgType -> ColRef -> Cmd Text
colBlogCont IMGtxt cr = do
  colImgOp iop cop cr
  where
    iop i n     = getColBlogCont (ImgRef i n)
    cop _       = return mempty
colBlogCont _ _ = return mempty

getColBlogCont :: ImgRef -> Cmd Text
getColBlogCont (ImgRef i n) = do
      p <- objid2path i
      -- subst the name by the part name
      -- and build a file path
      f <- toFilePath (substPathName n p)
      genBlogHtml f

getColBlogSource :: ImgRef -> Cmd Text
getColBlogSource (ImgRef i n) = do
  p <- objid2path i
  -- subst the name by the part name
  -- and build a file path
  f <- toFilePath (substPathName n p)
  genBlogText f

putColBlogSource :: Text -> ImgRef -> Cmd ()
putColBlogSource t (ImgRef i n) = do
  p <- objid2path i
  f <- toFilePath (substPathName n p)
  writeBlogText t f
  journalChange $ SaveBlogText i n t

-- ----------------------------------------
