{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Html.Photo2
where

import Catalog.Cmd
import Data.ImageStore
import Data.ImgTree
import Data.MetaData
import Data.Prim
import Catalog.Html.Templates.Photo2.AlbumPage
import Catalog.System.ExifTool
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
                 liftTA $ warn $ "evalTmpl: unknown template ref ignored: "
                                 ++ show (n ^. isoString)
                 empty
             )

-- ----------------------------------------

pagePathExpr :: Regex
pagePathExpr =
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

-- ----------------------------------------

url2confPathNo :: FilePath -> Cmd (String, ColRef Path)
url2confPathNo f =
  case matchSubexRE pagePathExpr f of
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

-- TODO : remove it
url2pathNo :: FilePath -> Cmd (ColRef Path)
url2pathNo f = snd <$> url2confPathNo f

-- ----------------------------------------

url2confObjId :: FilePath -> Cmd (String, ColRef ObjId)
url2confObjId f = do
  (c, p) <- url2confPathNo f
  mi     <- crPath2crObjId p
  i      <- fromJustCmd ("no entry found for href " ++ show f) mi
  return (c, i)

url2objId :: FilePath -> Cmd (ColRef ObjId)
url2objId f = snd <$> url2confObjId f

-- convert a path ref into an object ref
crPath2crObjId :: ColRef Path -> Cmd (Maybe (ColRef ObjId))
crPath2crObjId (p, cix) = do
  i <- fst <$> getIdNode' p
  normColRef (i, cix)

-- ----------------------------------------

-- normalize a colref
-- if the result sub index is Nothing, the ref points to a collection
-- else the ref points to an image

normColRef :: ColRef ObjId -> Cmd (Maybe (ColRef ObjId))
normColRef cr@(_i, Nothing) =
  return $ Just cr
normColRef cr@(i, Just pos) = do
  val <- getImgVal i
  case val ^? theColEntries . ix pos of
    Just (ImgRef _ _) ->  -- ref to an image
      return $ Just cr
    Just (ColRef j) ->    -- ref to a sub collection
      return $ Just (j, Nothing)
    Nothing ->            -- index out of bounds
      return Nothing

-- ----------------------------------------
--
-- navigation in the collection tree
--
-- 1 step up, 1 step down (indexed),
-- and to the left or right by an offset

parentColRef :: ColRef ObjId -> Cmd (Maybe (ColRef ObjId))
parentColRef (i, Nothing) = do
  parent'i <- getImgParent i
  iscol    <- isCOL <$> getImgVal parent'i
  return $
    if iscol
    then Just (parent'i, Nothing)
    else Nothing

parentColRef (i, Just _) =
  return $ Just (i, Nothing)

childColRef :: Int -> ColRef ObjId -> Cmd (Maybe (ColRef ObjId))
childColRef pos (i, Nothing) = do
  cs <- getImgVals i theColEntries
  case cs ^? ix pos of
    Just _ ->
      normColRef (i, Just pos)
    Nothing ->  -- index out of bounds
      return Nothing
childColRef _pos _ =
  return Nothing

ixColRef :: ColRef ObjId -> Cmd (Maybe Int)
ixColRef (_, Just pos) =
  return $ Just pos
ixColRef cr@(i, Nothing) = do
  mp'i <- parentColRef cr
  case mp'i of
    Nothing ->
      return $ Nothing
    Just (j, _) -> do
      cs <- getImgVals j theColEntries
      return $
        searchPos ((== i) . (^. theColObjId)) cs

neighborColRef :: Int -> ColRef ObjId -> Cmd (Maybe (ColRef ObjId))
neighborColRef offset cr@(_i, Nothing) = do -- col neighbor
  -- position in parent col
  mpos <- ixColRef cr
  case mpos of
    Nothing ->
      return Nothing
    Just pos -> do
      x <- parentColRef cr       -- 1 level up
      case x of
        Just parent'cr ->
          childColRef (pos + offset) parent'cr -- 1 level down
        _ ->
          return Nothing

neighborColRef offset (i, Just pos) = -- img neighbor
  normColRef (i, Just $ pos + offset)

prevColRef, nextColRef :: ColRef ObjId -> Cmd (Maybe (ColRef ObjId))
prevColRef = neighborColRef (-1)
nextColRef = neighborColRef   1

-- ----------------------------------------

-- the main entry for a HTML page

genHtmlPage :: FilePath -> Cmd Html
genHtmlPage p = do
  ( pageConf,
    this'cr@(this'i, _pos)) <- url2confObjId p

  (geo1, geo2, no'rows, env) <- fromJustCmd
    ("can't find config for " ++ show pageConf)
    (lookup pageConf thePageConfigs)


  -- pnp@(config, this'ref@(this'path, mno)) <- url2confPathNo p

  -- this entry
  this'img    <- colImgPath               this'cr
  this'cs     <- getImgVals this'i theColEntries  -- for a col: get the entries, for an img: empty
  this'meta   <- colImgMeta               this'cr

  parent'cr   <- parentColRef             this'cr
  parent'href <- colHref pageConf   `bmb` parent'cr
  parent'img  <- colImgPath         `bmb` parent'cr
  parent'meta <- colImgMeta         `bmb` parent'cr

  prev'cr     <- prevColRef               this'cr
  prev'href   <- colHref pageConf   `bmb` prev'cr
  prev'img    <- colImgPath         `bmb` prev'cr
  prev'meta   <- colImgMeta         `bmb` prev'cr

  next'cr     <- nextColRef               this'cr
  next'href   <- colHref pageConf   `bmb` next'cr
  next'img    <- colImgPath         `bmb` next'cr
  next'meta   <- colImgMeta         `bmb` next'cr

  child1'cr   <- childColRef 0            this'cr
  child1'href <- colHref pageConf   `bmb` child1'cr
  child1'img  <- colImgPath         `bmb` child1'cr
  child1'meta <- colImgMeta         `bmb` child1'cr

  let getTitle md   = md ^. metaDataAt "COL:Title" . isoString

  let this'title    = getTitle this'meta
  let this'subtitle = this'meta   ^. metaDataAt "COL:SubTitle" . isoString
  let this'resource = this'meta   ^. metaDataAt "COL:Resource" . isoString

  let parent'title  = getTitle parent'meta
  let prev'title    = getTitle prev'meta
  let next'title    = getTitle next'meta
  let child1'title  = getTitle child1'meta

  -- prev, next and parent
  -- pnp'paths@(prev'path, next'path, parent'path) <- pnpPaths this'ref

  -- let pnp'hrefs@(prev'href', next'href', parent'href') =
  --      (\ p' -> p' & (each . _Just) %~ path2href config) pnp'paths

  -- (parent'img',   prev'img',   next'img')   <- traverseOf each imgPath'  pnp'hrefs
  -- (parent'title', prev'title', next'title') <- traverseOf each getTitle' pnp'paths

  children'crs    <- mapM (flip childColRef this'cr) $
                     [0 .. length this'cs - 1]
  children'hrefs  <- mapM (colHref pageConf `bmb`) children'crs
  children'imgs   <- mapM (colImgPath       `bmb`) children'crs
  children'meta   <- mapM (colImgMeta       `bmb`) children'crs

  let children'titles = map getTitle children'meta
  let children'4      = zip4 children'hrefs children'imgs children'titles [(0::Int)..]

  -- children'hrefs  <- childrenPaths config this'i cs
  -- children'imgs   <- mapM imgPath'' children'hrefs
  -- children'titles <- sequence $ map (uncurry childTitle) $ zip [0..] cs
  -- let children = zip4 children'hrefs children'imgs children'titles [(0::Int)..]

  -- let child1'href'  = listToMaybe children'hrefs
  -- let child1'img'   = join $ listToMaybe children'imgs  -- join 2 Maybe's
  -- let child1'title' = listToMaybe children'titles

  let addColon x = if isempty x then x else ": "++ x

  let env' =
        env
        & addDefaultAct
        & insAct "rootPath"      (return "") -- (liftTA (use theMountPath) >>= tatt)
        & insAct "theUpPath"     (return "")
        & insAct "theDuration"   (return "1")
        & insAct "theDate"       (liftTA (show <$> atThisMoment) >>= tatt)
        & insAct "theTitle"      (tatt this'title)
        & insAct "theSubTitle"   (tatt this'subtitle)
        & insAct "theResource"   (tatt this'resource)
        & insAct "theHeadTitle"  (tatt this'title)

        -- the img geo
        & insAct "theImgGeoDir"  (tatt $ geo1 ^. isoString)
        & insAct "theIconGeoDir" (tatt $ geo2 ^. isoString)
        & insAct "theImgGeo"     (tatt $ geo1 ^. theGeo . isoString)
        & insAct "theIconGeo"    (tatt $ geo2 ^. theGeo . isoString)

        -- the href's
        & insAct "thisHref"      (tatt p)
        & insAct "thePrevHref"   (tatt prev'href)
        & insAct "theNextHref"   (tatt next'href)
        & insAct "theParentHref" (tatt parent'href)
        & insAct "theChild1Href" (tatt child1'href)

        -- the titles
        & insAct "theParentTitle" (tatt $ addColon parent'title)
        & insAct "theChild1Title" (tatt $ addColon child1'title)
        & insAct "thePrevTitle"   (tatt $ addColon prev'title)
        & insAct "theNextTitle"   (tatt $ addColon next'title)

        -- the img hrefs
        & insAct "thePrevImgRef"    (applyNotNull prev'href)
        & insAct "theNextImgRef"    (applyNotNull next'href)
        & insAct "theChild1ImgRef"  (applyNotNull child1'href)
        & insAct "parentImgRef"     (blankImg parent'img)
        & insAct "prevImgRef"       (blankImg prev'img)
        & insAct "nextImgRef"       (blankImg next'img)
        & insAct "child1ImgRef"     (blankImg child1'img)
        & insAct "colImg"           (applyNotNull this'img)

        -- the nav templates
        & insAct "parentNav"     (applyNotNull parent'href)
        & insAct "prevNav"       (applyNotNull prev'href)
        & insAct "nextNav"       (applyNotNull next'href)
        & insAct "child1Nav"     (applyNotNull child1'href)

        -- the contents, a nested loop over all children
        -- 4 infos are available per image, the "href"" for the image page,
        -- the the "src" of the image, the title, and the position in the collection

        & insAct "colContents"   (applyNotNull children'4) -- content only inserted when there are any entries
        & insAct "colRows" ( applySeqs
                             [ ( "colIcons"
                               , \ row ->
                                 applySeqs               -- generate a single row
                                 [ ( "theChildHref"
                                   , \ x -> tatt $ x ^. _1
                                   )
                                 , ( "theChildImgRef"
                                   , \ x -> blankImg $ x ^. _2
                                   )
                                 , ( "theChildTitle"
                                   , \ x -> tatt $
                                            let s = x ^. _3
                                                i = x ^. _4
                                            in if isempty s
                                               then show (i + 1) ++ ". Bild"
                                               else s
                                   )
                                 , ( "theChildId"
                                   , \ x -> tatt $ x ^. _4 . isoPicNo
                                   )
                                 ]
                                 row
                               )
                             ]
                             (divideAt no'rows children'4) -- divide entries into rows
                           )

  res <- runTmplAct applyTmpl "colPage" env'
  io $ putStrLn (mconcat res ^. isoString) -- readable test output
  return []

-- ----------------------------------------

bmb :: (Monad m, Monoid b) => (a -> m b) -> Maybe a -> m b
bmb cmd = maybe (return mempty) cmd

{-}
cmd :: ColRef ObjId -> Cmd (Maybe a)
cr  ::  Maybe (ColRef ObjId)
=>
cmd `bmb` cr :: Cmd (Maybe a)


-- -}
-- ----------------------------------------

childTitle :: Int -> ColEntry -> Cmd String
childTitle pos (ImgRef _ _) =
  return $ "Bild " ++ show (pos + 1)

childTitle pos (ColRef i) = do
  t <- getImgVals i (theColMetaData . metaDataAt "COL:Title" . isoString)
  return $
    if isempty t
    then "Album " ++ show (pos + 1)
    else t

-- ----------------------------------------

colHref :: String -> ColRef ObjId -> Cmd FilePath
colHref cf (i, cix) = do
  p <- objid2path i
  let fp =
        case cix of
          Nothing ->
            p ^. isoString
          Just pos ->
            (p ^. isoString) </> (pos ^. isoPicNo)
  return $ path2href cf fp

-- ----------------------------------------

colImgMeta :: ColRef ObjId -> Cmd MetaData
colImgMeta (i, Just pos) = do  -- img meta data
  cs <- getImgVals i theColEntries
  case cs ^? ix pos of
    Just (ImgRef j _n) ->
      getMetaData j
    _ ->
      return mempty

colImgMeta (i, Nothing) =     -- col meta data
  getImgVals i theColMetaData

-- ----------------------------------------

colImgPath :: ColRef ObjId -> Cmd (Maybe FilePath)
colImgPath (i, Just pos) = do  -- image ref
  cs <- getImgVals i theColEntries
  case cs ^? ix pos of
    Just (ImgRef j n) ->
      Just <$> thisImgPath j n
    _ ->
      return Nothing

colImgPath (i, Nothing) = do -- col ref
  j'img <- getImgVals i theColImg
  case j'img of
    -- collection has a front page image
    Just (k, n) ->
      Just <$> thisImgPath k n
    _ ->
      return Nothing

thisImgPath :: ObjId -> Name -> Cmd FilePath
thisImgPath i n = do
  this'p <- objid2path i
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

imgPath'' :: FilePath -> Cmd (Maybe FilePath)
imgPath'' f = url2pathNo f >>= imgPath

imgPath' :: Maybe FilePath -> Cmd (Maybe FilePath)
imgPath' Nothing  = return Nothing
imgPath' (Just f) = imgPath'' f

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
