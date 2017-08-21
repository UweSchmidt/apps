{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Catalog.Html.Basic
where

import Data.ImgTree
import Data.MetaData
import Data.Prim

import Catalog.Cmd
import Catalog.Journal        ( Journal'(SaveBlogText) )
import Catalog.System.Convert ( genAssetIcon
                              , genBlogText
                              , genBlogHtml
                              , writeBlogText
                              )

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
        Just (ImgRef j n) -> iop j n
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
      j'img <- getImgVals i theColImg
      case j'img of
        -- collection has a front page image
        Just (k, n) ->
          Just <$> buildImgPath0 k n
        _ ->
          return Nothing

    iop j n = Just <$> buildImgPath0 j n

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

buildImgPath0 :: ObjId -> Name -> Cmd FilePath
buildImgPath0 i n = do
  p <- objid2path i
  return $ substPathName n p ^. isoString

buildImgPath :: ObjId -> Name -> Cmd FilePath
buildImgPath i n = addJpg <$> buildImgPath0 i n

addJpg :: FilePath -> FilePath
addJpg f
  | ".jpg" `isSuffixOf` f = f
  | otherwise             = f ++ ".jpg"
    -- if the image isn't a .jpg (.png, .gif, ...) then a .jpg is added

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

path2img :: FilePath -> Cmd (Maybe FilePath)
path2img f
  | [("year", y)] <- m1 =
      genAssetIcon y y

  | [("year", y), ("month", m)] <- m1 =
      let s = toN m ++ "." ++ y
      in
        genAssetIcon (y </> s) s

  | [("year", y), ("month", m), ("day", d)] <- m1 =
      let s = toN d ++ "." ++ toN m ++ "." ++ y
      in
        genAssetIcon (y </> s) s

  | [("name", n)] <- m2 =
      genAssetIcon n n

{-
  | f == ps'bycreatedate =           -- "/archive/collections/byCreateDate"
      genAssetIcon s'bycreatedate (tt'bydate ^. isoString)

  | f == ps'clipboard =              -- "/archive/collections/photos/clipboard"
      genAssetIcon s'clipboard (tt'clipboard ^. isoString)

  | f == ps'trash =              -- "/archive/collections/photos/trash"
      genAssetIcon s'trash (tt'trash ^. isoString)

  | f == ps'photos =                 -- "/archive/collections/photos"
      genAssetIcon s'photos (tt'photos ^. isoString)

  | f == ps'collections =            -- "/archive/collections"
      genAssetIcon s'collections (tt'collections ^. isoString)
-- -}

  | otherwise =
      return Nothing
  where
    m1 = matchSubexRE ymdRE f
    m2 = matchSubexRE dirRE f

    toN :: String -> String
    toN s = show i
      where
        i :: Int
        i = read s


ymdRE :: Regex -- for collections sorted by date
ymdRE =
  parseRegexExt $
  ps'bycreatedate                    -- "/archive/collections/byCreateDate"
  ++
  "/({year}[0-9]{4})"
  ++
  "(/([-0-9]*({month}[0-9]{2}))(/([-0-9]*({day}[0-9]{2})))?)?"

dirRE :: Regex -- for collections for all folders
dirRE =
  parseRegexExt $
  ps'photos                          -- "/archive/collections/photos"
  ++
  "(/[^/]+)*"
  ++
  "(/({name}[^/]+))"

-- ----------------------------------------

colBlogCont :: ImgType -> ColRef -> Cmd Text
colBlogCont IMGtxt cr = do
  colImgOp iop cop cr
  where
    iop i n     = getColBlogCont i n
    cop _       = return mempty
colBlogCont _ _ = return mempty

getColBlogCont :: ObjId -> Name -> Cmd Text
getColBlogCont i n = do
      p <- objid2path i
      -- subst the name by the part name
      -- and build a file path
      f <- toFilePath (substPathName n p)
      genBlogHtml f

getColBlogSource :: ObjId -> Name -> Cmd Text
getColBlogSource i n = do
  p <- objid2path i
  -- subst the name by the part name
  -- and build a file path
  f <- toFilePath (substPathName n p)
  genBlogText f

putColBlogSource :: Text -> ObjId -> Name -> Cmd ()
putColBlogSource t i n = do
  p <- objid2path i
  f <- toFilePath (substPathName n p)
  writeBlogText t f
  journalChange $ SaveBlogText i n t

-- ----------------------------------------
