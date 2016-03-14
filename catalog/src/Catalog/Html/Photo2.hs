{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Html.Photo2
where

import Catalog.Cmd
import Catalog.FilePath
import Data.ImageStore
import Data.ImgTree
import Data.Prim
import Catalog.Html.Templates.Photo2.AlbumPage
import Text.SimpleTemplate

-- ----------------------------------------

type Html = [Text]

-- a page config has an name, a geo for the pictures,
-- a geo for the icons,
-- and the # icons per row

type PageConfig = (Text, (GeoAR, GeoAR, Int, EnvTmpl Cmd))

type ActCmd = TmplAct Cmd

-- ----------------------------------------

thePageConfigs :: [PageConfig]
thePageConfigs =
  [ ("html-1600x1200", ( GeoAR 1600 1200 Pad
                       , GeoAR   160 120 Fix
                       , 9
                       , photo2Tmpl
                       )
    )
  , ("html-1400x1050", ( GeoAR 1400 1050 Pad
                       , GeoAR  160  120 Fix
                       , 9
                       , photo2Tmpl
                       )
    )
  , ("html-1024x768",  ( GeoAR 1024  768 Pad
                       , GeoAR  160  120 Fix
                       , 9
                       , photo2Tmpl
                       )
    )
  ]

-- ----------------------------------------

defaultAct :: ActCmd
defaultAct n env = do
  warn $ "evalTemplate: unknown template ref ignored" ++ show (n ^. isoString)
  return ["${" <> n <> "}"]

addDefaultAct :: EnvTmpl Cmd -> EnvTmpl Cmd
addDefaultAct = insAct "*" defaultAct

-- ----------------------------------------

pathExpr :: Regex
pathExpr =
  parseRegexExt $
  "/({config}[a-z]+-[0-9]+x[0-9]+)" ++
  "(" ++
  "(({path}/archive/collections/.*)(/pic-({no}[0-9]+))[.]html)" ++
  "{|}" ++
  "(({path}/archive/collections/.*)[.]html)" ++
  ")"

url2confPathNo :: FilePath -> Cmd (Text, Path, Maybe Int)
url2confPathNo f =
  case matchSubexRE pathExpr f of
    [("config", config), ("path", path), ("no", no)] ->
      return ( config ^. isoText
             , path ^. from isoString
             , Just $ read no
             )
    [("config", config), ("path", path)] ->
      return ( config ^. isoText
             , path ^. from isoString
             , Nothing
             )
    _ -> abort $ "can't process document ref " ++ show f

genHtmlPage :: FilePath -> Cmd Html
genHtmlPage p = do
  mountPath <- use theMountPath
  (config, path, mno) <- url2confPathNo p
  (geo1, geo2, rows, env) <- fromJustCmd
    ("can't find config for " ++ config ^. isoString)
    (lookup config thePageConfigs)

  let env' =
        env
        & addDefaultAct
        & insAct "rootPath"    (atxt mountPath)  -- TODO
        & insAct "theUpPath"   (atxt "")
        & insAct "theDuration" (atxt "1")
        & insAct "theImgGeo"   (atxt $ geo1 ^. isoString)
        & insAct "theIconGeo"  (atxt'$ return geo2)

  res <- applyTmpl "colPage" env'
  io $ putStrLn $ (mconcat res ^. isoString)
  return []

type Neighbors a = (Maybe a, Maybe a, Maybe a) -- prev, next, parent

allNeighbors :: Traversal (Neighbors a) (Neighbors b) a b
allNeighbors = each . _Just
{-# INLINE allNeighbors #-}

neighborPaths :: Neighbors ObjId -> Cmd (Neighbors Path)
neighborPaths = traverseOf allNeighbors objid2path

htmlCollection :: Path -> Cmd Html
htmlCollection col'path = do
  (i, val)  <- getIdNode "htmlCollection: collection not found" col'path
  unless (isCOL val) $
    abort $ "htmlCollection: not a collection" ++ show (show col'path)
  htmlCol i


htmlCol :: ObjId -> Cmd Html
htmlCol i = do
  -- the path of the collection
  i'p <- objid2path i

  -- the ids of the neighbors, and the pos in parent collection
  -- neded for nav info
  (neighbor'is, _pos) <- getNeighbors i

  -- the paths of the neighbors, needed in href's
  neighbor'ps         <- traverseOf allNeighbors objid2path neighbor'is

  -- the image parts of the neighbors, needed for img refs
  -- and the names of the neighbors
  neighbos'ims        <- traverseOf allNeighbors getImgParts neighbor'is

  -- the meta data collection image and contents
  (COL md mbi cs _ts) <- getImgVal i

  -- the image parts and the names of all images in the collection
  cs'ims              <- traverseOf traverse getColParts cs

  return undefined

getImgParts :: ObjId -> Cmd (ImgParts, Name)
getImgParts = undefined

getColParts :: ColEntry -> Cmd (Path, (ImgParts, Name))
getColParts = undefined

getNeighbors :: ObjId -> Cmd (Neighbors ObjId, Int)
getNeighbors i = do
  i'p <- getImgParent i
  v'p <- getImgVal i'p
  if (not $ isCOL v'p)
    then return ((Nothing, Nothing, Nothing), 0)  -- top collection hasn't a parent coll nor neighbors
    else do
      let ce = v'p ^. theColEntries
      case searchPos (\c' -> c' ^. theColObjId == i) ce of
        Just 0 ->
          return ((Nothing, ref1 $ tail ce, Just i'p), 0)
        Just i' ->
          let ce1 = drop (i' - 1) ce
              r1  = ref1 ce1
              r2  = ref1 (drop 2 ce1)
          in return ((r1, r2, Just i'p), i')
        Nothing -> do
          p <- objid2path i
          abort $ "getNeighbors: ObjId not found in parent collection" ++ show (show p)
  where
    ref1 ce' = (^. theColObjId) <$> listToMaybe ce'
