{-# LANGUAGE OverloadedStrings #-}

module Catalog.Cmd.ArchiveCollection
       ( genSysCollections
       , genImportsCollection
       , genAllCollectionsByDir
       , genCollectionsByDir
       , genCollectionsByDir'
       , updateCollectionsByDate
       , img2colPath
       )
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Catalog.Cmd.CopyRemove (removeEmptyColls)
import           Catalog.FilePath (pathToBreadCrump)
import           Catalog.System.ExifTool
import           Catalog.System.IO
import           Data.ImgTree
-- import           Data.ImgNode
import           Data.MetaData
import           Data.Prim
import qualified Data.IntMap as IM

-- ----------------------------------------

genSysCollections :: Cmd ()
genSysCollections = do
  verbose "genSysCollections: create/update system collections (clipboard, albums, imports)"

  -- the collection root is already there
  -- just set the meta data
  genCollectionRootMeta

  -- gen clipboard albums and imports, if not already there
  genClipboardCollection
  genPhotoCollection
  genAlbumsCollection
  genByDateCollection
  genImportsCollection

genCollectionRootMeta :: Cmd ()
genCollectionRootMeta = do
  ic <- getRootImgColId
  md <- mkColMeta t s c o a
  adjustMetaData (md <>) ic
  where
    t = tt'collections
    s = ""
    c = ""
    o = ""
    a = no'change

-- create the special collections for clipboard and trash

genClipboardCollection :: Cmd ()
genClipboardCollection = genSysCollection no'delete n'clipboard tt'clipboard

genPhotoCollection :: Cmd ()
genPhotoCollection = genSysCollection no'change n'photos tt'photos

genAlbumsCollection :: Cmd ()
genAlbumsCollection = genSysCollection no'restr n'albums tt'albums

genImportsCollection :: Cmd ()
genImportsCollection = genSysCollection no'restr n'imports tt'imports

genByDateCollection :: Cmd ()
genByDateCollection = genSysCollection no'change n'bycreatedate tt'bydate

genSysCollection :: Text -> Name -> Text -> Cmd ()
genSysCollection a n'sys tt'sys = do
  ic <- getRootImgColId
  pc <- objid2path ic
  let sys'path = pc `snocPath` n'sys
  ex <- lookupByPath sys'path
  case ex of
    Nothing -> do
      void $ mkColByPath insertColByName setupSys sys'path
    Just _ ->
      return ()
  where
    setupSys _i =
      mkColMeta t s c o a
      where
        t = tt'sys
        s = ""
        c = ""
        o = ""

-- create directory hierachy for Y/M/D
mkDateCol :: (String, String, String) -> Path -> Cmd (ObjId, ObjId, ObjId)
mkDateCol (y, m, d) pc = do
  yc <- mkColByPath insertColByName (setupYearCol  y    ) py
  mc <- mkColByPath insertColByName (setupMonthCol y m  ) pm
  dc <- mkColByPath insertColByName (setupDayCol   y m d) pd
  return (yc, mc, dc)
  where
    py = pc `snocPath` mkName y
    pm = py `snocPath` mkName (y ++ "-" ++ m)
    pd = pm `snocPath` mkName (y ++ "-" ++ m ++ "-" ++ d)

    setupYearCol y' _i =
      mkColMeta t "" "" o a
        where
          t = tt'year y'
          o = to'name
          a = no'change

    setupMonthCol y' m' _i =
      mkColMeta t "" "" o a
        where
          t = tt'month y' m'
          o = to'name
          a = no'change

    setupDayCol y' m' d' _i =
      mkColMeta t "" "" o a
        where
          t = tt'day y' m' d'
          o = to'dateandtime
          a = no'change

-- ----------------------------------------

-- gen collection for whole img hierachy
--
-- the collection of all images in archive is generated
-- in the collection root with the same name as the archive
-- root.
--
-- The collections are sorted by subcollections first and then by name
--
-- The collections are updatet only if the corresponding archive
-- dir is newer than the collection. This makes an update pretty fast

img2colPath :: Cmd (Path -> Path)
img2colPath = do
  pc <- getRootImgColId >>= objid2path -- the collection root path

  -- create root collection for archive dir hierachy
  let (rootName, pc1) = pc  ^. viewTop
  let (colName, _pc2) = pc1 ^. viewTop
  let old'px  = mkPath rootName
  let new'px  = rootName `consPath` mkPath colName
  return $ substPathPrefix old'px new'px

genAllCollectionsByDir :: Cmd ()
genAllCollectionsByDir =
  getRootImgDirId >>= genCollectionsByDir

genCollectionsByDir' :: Path -> Cmd ()
genCollectionsByDir' p = do
  mbi <- lookupByPath p
  maybe (return ()) (genCollectionsByDir . fst) mbi

genCollectionsByDir :: ObjId -> Cmd ()
genCollectionsByDir di = do
  img2col <- img2colPath
  dp      <- objid2path di
  void $ mkColByPath insertColByName setupDirCol (img2col dp)
  void $ genCol img2col di
  where
    -- meta data for generated collections
    setupDirCol :: ObjId -> Cmd MetaData
    setupDirCol i = do
      p  <- tailPath . tailPath <$> objid2path i
      let t = path2Title p
          s = path2Subtitle p
          o = to'colandname
          a = no'wrtdel
      mkColMeta t s "" o a

    path2Title :: Path -> Text
    path2Title p
      | isempty b && n == n'photos =
          tt'photos ^. isoText
      | otherwise =
          n ^. isoText
      where
        (b, n) = p ^. viewBase

    path2Subtitle :: Path -> Text
    path2Subtitle p
      | isempty bs = mempty
      | otherwise  = (pathToBreadCrump . show) p ^. isoText
      where
        bs = p ^. viewBase . _1

    genCol :: (Path -> Path) -> ObjId -> Cmd [ColEntry]
    genCol fp =
      foldImgDirs imgA dirA
      where
        -- collect all processed jpg images for a single img

        imgA i pts _md = do
          let res = map (mkColImgRef i) $ sort ns
          trcObj i $ "genCol img: " ++ show res
          return res
          where
            ns = pts ^.. thePartNamesI

        -- generate a coresponding collection with all entries
        -- entries are sorted by name

        dirA :: (ObjId -> Cmd [ColEntry]) ->
                ObjId -> DirEntries -> TimeStamp -> Cmd [ColEntry]
        dirA go i es _ts = do
          p  <- objid2path i
          let cp = fp p
          trcObj i $ "genCol dir " ++ show cp

          -- check or create collection
          -- with action for meta data
          ic <- mkColByPath insertColByName setupDirCol cp

          -- get collection entries, and insert them into collection
          cs  <- concat <$> mapM go (es ^. isoDirEntries)

          trcObj ic "genCol dir: set dir contents"
          adjustColByName cs ic
          trcObj ic "genCol dir: dir contents is set"

          -- set the blog entry, if there's a txt entry in cs
          setColBlogToFstTxtEntry False ic
          trcObj ic "genCol dir: col blog set"

          return [mkColColRef ic]

-- ----------------------------------------
--
-- collection sort

sortByName :: [ColEntry] -> Cmd [ColEntry]
sortByName =
  sortColEntries getVal compare
  where

    -- collections come first and are sorted by name
    -- images are sorted by name and part name
    getVal :: ColEntry -> Cmd (Either Name (Name, Name))
    getVal =
      colEntry
      (\ j n1 -> (\ n -> Right (n, n1))  <$> getImgName j)
      (\ j    ->         Left            <$> getImgName j)


sortByDate :: [ColEntry] -> Cmd [ColEntry]
sortByDate =
  sortColEntries getVal compare
  where
    -- collections come first, should be redundant,
    -- there should be only images in the collection of a day
    --
    -- the images are sorted by creation time and
    -- if that fails, by name

    getVal =
      colEntry
      (\ j n1 -> do
          md  <- getMetaData j
          let t = getCreateMeta parseTime md
          return $ Right (t, n1)
      )
      (\ j    -> Left <$> getImgName j )

-- ----------------------------------------
--
-- set/modify collection entries

insertColByName :: ObjId -> ObjId -> Cmd ()
insertColByName i = adjustColByName [mkColColRef i]

-- insertColByDate :: ObjId -> ObjId -> Cmd ()
-- insertColByDate i = adjustColByDate [mkColColRef i]

adjustColByName :: [ColEntry] -> ObjId -> Cmd ()
adjustColByName = adjustColBy sortByName

adjustColByDate :: [ColEntry] -> ObjId -> Cmd ()
adjustColByDate = adjustColBy sortByDate

adjustColBy :: ([ColEntry] -> Cmd [ColEntry]) ->
               [ColEntry] ->
               ObjId -> Cmd ()
adjustColBy sortCol cs parent'i = do
  -- trc $ "adjustColBy begin"
  cs'old <- getImgVals parent'i theColEntries
  -- trc $ "adjustColBy" ++ show cs'old
  cs'new <- sortCol $ cs'old `mergeColEntries` cs
  -- trc $ "adjustColBy" ++ show cs'new
  adjustColEntries (const cs'new) parent'i
  -- trc $ "adjustColBy end"

-- ----------------------------------------

findFstTxtEntry :: ObjId -> Cmd (Maybe (Int, ColEntry))
findFstTxtEntry = findFstColEntry isTxtEntry
  where
    isTxtEntry =
      colEntry
      (\ i n -> do
          nd <- getImgVal i
          let ty = nd ^? theParts . isoImgPartsMap . ix n . theImgType
          return $ maybe False (== IMGtxt) ty
      )
      (const $ return False)

-- take the 1. text entry in a collection
-- and set the collection blog entry to this value
-- rm indicates, whether the entry is removed from the collection

setColBlogToFstTxtEntry :: Bool -> ObjId -> Cmd ()
setColBlogToFstTxtEntry rm i = do
  fte <- findFstTxtEntry i
  maybe (return ()) setEntry fte
  where
    setEntry (pos, (ImgEnt ir)) = do
      trc $ unwords ["setColBlogToFstTxtEntry", show i, show pos, show ir]
      adjustColBlog (const $ Just ir) i
      when rm $
        remColEntry pos i
    setEntry _ =
      return ()

-- ----------------------------------------

mkColMeta :: Text -> Text -> Text -> Text -> Text -> Cmd MetaData
mkColMeta t s c o a = do
  d <- (\ t' -> show t' ^. isoText) <$> atThisMoment
  return $
      mempty
      & metaDataAt descrTitle      .~ t
      & metaDataAt descrSubtitle   .~ s
      & metaDataAt descrComment    .~ c
      & metaDataAt descrOrderedBy  .~ o
      & metaDataAt descrCreateDate .~ d
      & metaDataAt descrAccess     .~ a

-- create collections recursively, similar to 'mkdir -p'

mkColByPath :: (ObjId -> ObjId -> Cmd ()) ->
               (ObjId -> Cmd MetaData) -> Path -> Cmd ObjId
mkColByPath insertCol setupCol p = do
  -- trc $ "mkColByPath " ++ show p
  -- check for legal path
  when (isempty $ tailPath p) $
    abort $ "mkColByPath: can't create collection " ++ quotePath p

  mid <- lookupByPath p
  cid <-
    case mid of
      -- collection does not yet exist
      Nothing -> do
        -- compute (create) parent collection(s)
        -- meta data of parent collections remains unchanged
        let (p1, n) = p ^. viewBase
        ip <- mkColByPath insertCol (const $ return mempty) p1
        verbose $ "mkColByPath " ++ show p1 ++ "/" ++ show n
        -- create collection
        ic <- mkImgCol ip n
        -- inser collection into parent collection
        insertCol ic ip
        return ic

      -- entry already there
      Just (ip, vp) -> do
        unless (isCOL vp) $
          abort $ "mkColByPath: can't create collection, other entry already there " ++
                  quotePath p
        return ip

  -- meta data update always done,
  -- neccessary if the titile generation has been modified
  md <- setupCol cid
  adjustMetaData (md <>) cid
  return cid

-- ----------------------------------------

type DateMap = IM.IntMap ColEntrySet

updateCollectionsByDate :: ColEntrySet -> Cmd ()
updateCollectionsByDate rs = do
  verbose $ "updateCollectionsByDate: new refs are added to byDate collections: "
            ++ show rs
  dm <- colEntries2dateMap rs
  dateMap2Collections p'bycreatedate dm

  verbose "cleanup bydate collections"
  removeEmptyColls p'bycreatedate


-- group col entries by create date

colEntries2dateMap :: ColEntrySet -> Cmd DateMap
colEntries2dateMap rs = do
  verbose "colEntries2dateMap: build DateMap"
  foldlM add1 IM.empty $ toListColEntrySet rs
  where
    add1 :: DateMap -> ColEntry -> Cmd DateMap
    add1 acc ce = do
      meta <- getMetaData (ce ^. theColObjId)
      let mdate = (^. isoDateInt) <$> getCreateMeta parseDate meta
      return $
        maybe acc
        (\ i' -> IM.insertWith (<>) i' (singletonColEntrySet ce) acc)
        mdate

-- create/update day collection with a col entry sets
-- pc is the path to the y/m/d collection hierachy

dateMap2Collections :: Path -> DateMap -> Cmd ()
dateMap2Collections pc dm =
  mapM_ insCol $ IM.toList dm
  where
    insCol (i, ces) = do
      (_yc, _mc, dc) <- mkDateCol ymd pc
      adjustColByDate cs dc
      verbose $ "dateMap2Collections: collection updated: " ++ show ymd
      return ()
      where
        ymd = i ^. from isoDateInt
        cs  = toListColEntrySet ces

-- ----------------------------------------
