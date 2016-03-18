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

type PageConfig = (String, (GeoAR, GeoAR, Int, TmplEnv Cmd))

-- an entry in a collection is identified by an ObjId, Path, Filepath, ...
-- and, an index into the list of collection entries, or it's a reference
-- to the collection itself

type ColRef a   = (a, Maybe Int)

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

addDefaultAct :: TmplEnv Cmd -> TmplEnv Cmd
addDefaultAct =
  insAct "*" (do n <- askTmplName
                 liftTA $ warn $ "evalTmpl: unknown template ref ignored" ++ show (n ^. isoString)
                 empty
             )

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

isoPicNo :: Iso' Int String
isoPicNo = iso toS frS
  where
    toS =
      ("pic-" ++ ) . reverse . take 4 . reverse . ("0000" ++ ). show
    frS s =
      case matchSubex "pic-({no}[0-9]+)" s of
        [("no", no)] ->
          read no
        _ -> -1

url2confPathNo :: FilePath -> Cmd (String, ColRef Path)
url2confPathNo f =
  case matchSubexRE pathExpr f of
    [("config", config), ("path", path), ("no", no)] ->
      return ( config
             , ( path ^. from isoString
               , Just $ read no
               )
             )
    [("config", config), ("path", path)] ->
      return ( config
             , ( path ^. from isoString
               , Nothing
               )
             )
    _ -> abort $ "can't process document ref " ++ show f

url2pathNo :: FilePath -> Cmd (ColRef Path)
url2pathNo f = snd <$> url2confPathNo f

getXXX = genHtmlPage "/html-1600x1200/archive/collections/photos/2015/pic-0001.html"
getYYY = genHtmlPage "/html-1600x1200/archive/collections/photos/2015.html"

genHtmlPage :: FilePath -> Cmd Html
genHtmlPage p = do
  pnp@(config, this'ref@(this'path, mno)) <- url2confPathNo p
  (geo1, geo2, rows, env) <- fromJustCmd
    ("can't find config for " ++ show config)
    (lookup config thePageConfigs)

  -- this entry
  this'i    <- colref2objid this'ref
  this'img  <- colImgPath   this'i mno
  this'val  <- getImgVal    this'i

  -- prev, next and parent
  pnp'hrefs@(prev'href, next'href, parent'href) <-
    (\ p' -> p' & (each . _Just) %~ path2href config) <$>
    pnpPaths this'ref

  (parent'img, prev'img, next'img) <- traverseOf each imgPath' pnp'hrefs

  let cs  = this'val ^. theColEntries
  let cs1 = cs ^? ix 0

  child1'href <- return Nothing -- maybe (return Nothing) (\ ) $ cs1
  child1'img <- return Nothing  -- TODO

  let addGeo2 x = fromMaybe "" ((("/" ++ geo2 ^. isoString) ++) <$> x)

  let env' =
        env
        & addDefaultAct
        & insAct "rootPath"      (liftTA (use theMountPath) >>= tatt)
        & insAct "theUpPath"     (return "")
        & insAct "theDuration"   (return "1")

        -- the img geo
        & insAct "theImgGeoDir"  (tatt $ geo1 ^. isoString)
        & insAct "theIconGeoDir" (tatt $ geo2 ^. isoString)
        & insAct "theImgGeo"     (tatt $ geo1 ^. theGeo . isoString)
        & insAct "theIconGeo"    (tatt $ geo2 ^. theGeo . isoString)

        -- the href's
        & insAct "thisHref"      (tatt $ p)
        & insAct "thePrevHref"   (tatt $ fromMaybe "" prev'href)
        & insAct "theNextHref"   (tatt $ fromMaybe "" next'href)
        & insAct "theParentHref" (tatt $ fromMaybe "" parent'href)
        & insAct "theChild1Href" (tatt $ fromMaybe "" child1'href)
        -- & insAct "theChildHref"  (tatt $ fromMaybe "" child1'href)

        -- the img hrefs
        & insAct "thePrevImgRef" (applyNotNull prev'img)
        & insAct "theNextImgRef" (applyNotNull next'img)
        & insAct "parentImgRef"  (blankImg parent'img)
        & insAct "prevImgRef"    (blankImg prev'img)
        & insAct "nextImgRef"    (blankImg next'img)
        & insAct "child1ImgRef"  (blankImg child1'img)
        & insAct "colImg"        (applyNotNull this'img)

        -- the nav templates
        & insAct "parentNav"     (applyNotNull parent'href)
        & insAct "prevNav"       (applyNotNull prev'href)
        & insAct "nextNav"       (applyNotNull next'href)
        & insAct "child1Nav"     (applyNotNull child1'href)


        -- & insAct "thisImgRef"    (tatt $ addGeo2 this'img)
        -- $ insAct "colImg"        undefined -- (\ n e -> if null this'img then undefined else applyTmpl n e)
        -- & insAct "parentImgRef"  undefined -- (tatt $ addGeo2 parent'img)

  res <- runTmplAct applyTmpl "colPage" env'
  io $ putStrLn (mconcat res ^. isoString) -- readable test output
  return []

-- ----------------------------------------

colImgPath :: ObjId -> Maybe Int -> Cmd (Maybe FilePath)

-- reference of an entry in a collection
colImgPath this'i (Just pos) = do
  cs <- getImgVals this'i theColEntries
  case cs ^? ix pos of
    -- this ref is an image
    Just (ImgRef j n) ->
      Just <$> thisImgPath j n

    -- this ref is a collection
    Just (ColRef j) ->
      colImgPath j Nothing

    -- this ref isn't there
    Nothing ->
      return Nothing

-- reference of the collection itself
colImgPath this'i Nothing = do
  j'img <- getImgVals this'i theColImg
  case j'img of
    -- collection has a front page image
    Just (k, n) ->
      Just <$> thisImgPath k n
    Nothing ->
      return Nothing

thisImgPath :: ObjId -> Name -> Cmd FilePath
thisImgPath this'i n = do
  this'p <- objid2path this'i
  return $ substPathName n this'p ^. isoString

blankImg :: Maybe FilePath -> ActCmd Text
blankImg f = (liftTA $ blankImg' f) >>= tatt

blankImg' :: Maybe FilePath -> Cmd FilePath
blankImg' =
  maybe ((++ "/icons/blank.jpg") <$> view envAssets) return

-- ----------------------------------------

type Neighbors a = (Maybe a, Maybe a, Maybe a)

-- allNeighbors :: Traversal (Neighbors a) (Neighbors b) a b
-- allNeighbors = each . _Just
-- {-# INLINE allNeighbors #-}

-- ----------------------------------------

colref2objid :: ColRef Path -> Cmd ObjId
colref2objid (p, Nothing) =
  fst <$> getIdNode' p

colref2objid (p, Just pos) = do
  n <- snd <$> getIdNode' p
  fromJustCmd
    ("collection expected with at least " ++ show (pos + 1) ++ " entries")
    (n ^? theColEntries . ix pos . theColObjId)

-- compute the navigation hrefs for previous, next and parent image/collection

path2href :: String -> FilePath -> FilePath
path2href c p = "/" ++ c ++ p ++ ".html"

childrenPaths :: String -> ObjId -> [ColEntry] -> Cmd [FilePath]
childrenPaths c i cs = do
  p <- objid2path i
  return $
    map ( path2href c
          .
          (\ pos -> p ^. isoString </> (pos ^. isoPicNo))
        )
    [0 .. length cs -1]

pnpPaths :: ColRef Path -> Cmd (Neighbors FilePath)
pnpPaths (p, Just i0) = do
  es <- (^. _2 . theColEntries) <$> getIdNode' p
  return
    ( ixP (i0 - 1) es
    , ixP (i0 + 1) es
    , parentP
    )
    where
      ixP :: Int -> [ColEntry] -> Maybe FilePath
      ixP i xs =
        (const $ (p ^. isoString) </> (i ^. isoPicNo)) <$> (xs ^? ix i)

      parentP :: Maybe FilePath
      parentP =
        Just (p ^. isoString)

pnpPaths (p, Nothing) = do
  this'i   <- fst <$> getIdNode' p
  parent'i <- getImgParent this'i
  parent'v <- getImgVal    parent'i
  parent'p <- objid2path   parent'i
  if not $ isCOL parent'v
    then return (Nothing, Nothing, Nothing)
    else do
      let parent's   = parent'p ^. isoString
      let parent'cs  = parent'v ^. theColEntries
      let pcs'length = length parent'cs
      case searchPos ((== this'i) . (^. theColObjId)) parent'cs of
        Just 0 ->
          return
            ( Nothing
            , if pcs'length > 1
              then Just $ parent's </> (1::Int) ^. isoPicNo
              else Nothing
            , Just parent's
            )
        Just i ->
          return
            ( Just $ parent's </> (i - 1) ^. isoPicNo
            , if i < pcs'length - 1
              then Just $ parent's </> (i + 1) ^. isoPicNo
              else Nothing
            , Just parent's
            )
        _ -> return (Nothing, Nothing, Nothing)

imgPath' :: Maybe FilePath -> Cmd (Maybe FilePath)
imgPath' Nothing =
  return Nothing

imgPath' (Just f) =
  url2pathNo f >>= imgPath


imgPath :: ColRef Path -> Cmd (Maybe FilePath)
imgPath (p, Nothing) = do
  v <- snd <$> getIdNode' p
  case v ^. theColImg of
    Nothing ->
      return Nothing
    Just (j, jn) -> do
      jp <- objid2path j
      return $ Just (substPathName jn jp ^. isoString)

imgPath (p, Just pos) = do
  mce <- (^? _2 . theColEntries . ix pos) <$> getIdNode' p
  case mce of
    Nothing ->
      return Nothing
    Just (ImgRef j jn) -> do
      jp <- objid2path j
      return $ Just (substPathName jn jp ^. isoString)
    Just (ColRef c) -> do
      p' <- objid2path c
      imgPath (p', Nothing)

-- ----------------------------------------

{- }
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

-- -}
