{-# LANGUAGE OverloadedStrings #-}

module Catalog.Cmd.ArchiveCollection
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Catalog.FilePath (pathToBreadCrump)
import           Catalog.System.ExifTool
import           Catalog.System.IO
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------

genSysCollections :: Cmd ()
genSysCollections = do
  verbose "genSysCollections: create/update system collections (clipboard, albums, imports)"

  -- the collection root is already there
  -- just set the meta data
  genCollectionRootMeta

  -- gen clipboard albums and imports, if not already there
  genClipboardCollection
  genAlbumsCollection
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

genAlbumsCollection :: Cmd ()
genAlbumsCollection = genSysCollection no'restr n'albums tt'albums

genImportsCollection :: Cmd ()
genImportsCollection = genSysCollection no'restr n'imports tt'imports

-- at the moment not in use
genTrashCollection :: Cmd ()
genTrashCollection = genSysCollection no'delete n'trash tt'trash

genSysCollection :: Text -> Name -> Text -> Cmd ()
genSysCollection a n'sys tt'sys = do
  ic <- getRootImgColId
  pc <- objid2path ic
  let sys'path = pc `snocPath` n'sys
  ex <- lookupByPath sys'path
  case ex of
    Nothing -> do
      sys'i <- mkColByPath insertColByName setupSys sys'path
      setSyncTime sys'i
    Just _ ->
      return ()
  where
    setupSys _i = do
      mkColMeta t s c o a
      where
        t = tt'sys
        s = ""
        c = ""
        o = ""

-- TODO: use time stamp of dirs and collections to
-- skip unchanged dirs, similar to genCollectionsByDir

genCollectionsByDate :: Cmd ()
genCollectionsByDate = do
  ic <- getRootImgColId
  pc <- objid2path ic
  di <- getRootImgDirId

  -- create root col for year/month/day hierachy
  -- "/archive/collections/byCreateDate"

  let top'iPath = pc `snocPath` n'bycreatedate
  top'i <- mkColByPath insertColByName setupByDate top'iPath
  top'iSyncTime <- getImgVals top'i theSyncTime

  -- fill the year/month/day hierachy
  processNewImages top'iSyncTime top'iPath di

  setSyncTime top'i
  where

    setupByDate :: ObjId -> Cmd MetaData
    setupByDate _i = do
      mkColMeta t s c o a
      where
        t = tt'bydate
        s = ""
        c = ""
        o = to'colandname
        a = no'change

processNewImages :: TimeStamp -> Path -> ObjId -> Cmd ()
processNewImages colSyncTime pc i0 = do
  is <- partBy fst <$> processImgDirs imgA dirA i0
  mapM_ addToCol is
  where
    -- skip unchanged dirs
    dirA go _i es dirSyncTime = do
      es' <- if False -- colSyncTime >= dirSyncTime
             then
               getImgSubDirs es
             else
               return (es ^. isoDirEntries)
      concat <$> mapM go es'

    -- read the jpg image part and the create date meta tag
    -- and build a list of pairs of date and ColImgRef's
    imgA :: ObjId -> ImgParts -> MetaData ->
            Cmd [((String, String, String), ColEntry)]
    imgA i pts _md = do
      md <- getMetaData i
      let mymd = getCreateMeta parseDate md
      -- trcObj i $ "processnewimages: mymd: " ++ show mymd
      case mymd of
        Nothing ->
          return []
        Just ymd -> do
          -- let res = [(ymd, mkColImgRef i n) | n <- ns]
          -- trcObj i $ "processnewimages res: " ++ show res
          return [(ymd, mkColImgRef i n) | n <- ns]
      where
        ns = pts ^.. thePartNamesI

    -- add all images for 1 day into the corresponding collection
    addToCol :: [((String, String, String), ColEntry)] -> Cmd ()
    addToCol dcs = do
      let ymd = fst . head $ dcs
          cs  = map snd dcs
      -- check or create y/m/d hierachy
      (yc, mc, dc) <-mkDateCol ymd
      -- trcObj dc $ "addToCol " ++ show (length cs) ++ " new images in"
      adjustColByDate cs dc
      setSyncTime yc >> setSyncTime mc >> setSyncTime dc
      where

    -- create directory hierachy for Y/M/D
    mkDateCol :: (String, String, String) -> Cmd (ObjId, ObjId, ObjId)
    mkDateCol (y, m, d) = do
      yc <- mkColByPath insertColByName (setupYearCol  y    ) py
      mc <- mkColByPath insertColByName (setupMonthCol y m  ) pm
      dc <- mkColByPath insertColByName (setupDayCol   y m d) pd
      return (yc, mc, dc)
      where
        py = pc `snocPath` mkName y
        pm = py `snocPath` mkName (y ++ "-" ++ m)
        pd = pm `snocPath` mkName (y ++ "-" ++ m ++ "-" ++ d)

        setupYearCol y' _i = do
          mkColMeta t "" "" o a
          where
            t = tt'year y'
            o = to'name
            a = no'change

        setupMonthCol y' m' _i = do
          mkColMeta t "" "" o a
          where
            t = tt'month y' m'
            o = to'name
            a = no'change

        setupDayCol y' m' d' _i = do
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
genAllCollectionsByDir = do
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
  -- adjustColEntries (`mergeColEntries` es) ic
  -- trc $ "genCollectionsbydir: " ++ show es
  -- return ()
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

    -- TODO (really)
    -- search for a IMGtxt entry in DIR entries,
    -- take the 1. as blog entry for the collection
    -- setupDirTxt :: ObjId -> Cmd (Maybe (ObjId, Name))
    -- setupDirTxt = undefined

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
      processImgDirs imgA dirA
      where
        -- collect all processed jpg images for a single img

        imgA i pts _md = do
          let res = (map (mkColImgRef i) $ sort ns)
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

          dirSyncTime <- getImgVals i  theSyncTime
          colSyncTime <- getImgVals ic theSyncTime

          if False -- colSyncTime >= dirSyncTime
            then do
              -- the collection is up to date
              -- only the subdirs need to be traversed
              trcObj i "genCol dir: dir is up to date, traversing subdirs"
              cs  <- getImgSubDirs es
              void $ mapM go cs
            else do
              -- get collection entries, and insert them into collection
              cs  <- concat <$> mapM go (es ^. isoDirEntries)

              trcObj ic "genCol dir: set dir contents"
              adjustColByName cs ic
              trcObj ic "genCol dir: dir contents is set"

              -- set the blog entry, if there's a txt entry in cs
              setColBlogToFstTxtEntry False ic
              trcObj ic "genCol dir: col blog set"

              -- set time processed
              setSyncTime ic

              -- TODO: recurce into subdirs ???

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
    getVal (ColRef j)    = Left                    <$> getImgName j
    getVal (ImgRef j n1) = (\ n -> Right (n, n1))  <$> getImgName j

sortByDate :: [ColEntry] -> Cmd [ColEntry]
sortByDate =
  sortColEntries getVal compare
  where
    -- collections come first, should be redundant,
    -- there should be only images in the collection of a day
    --
    -- the images are sorted by creation time and
    -- if that fails, by name

    getVal (ColRef j) =
      Left <$> getImgName j -- should never occur
    getVal (ImgRef j n1) = do
      md  <- getMetaData j
      let t = getCreateMeta parseTime md
      return $ Right (t, n1)

-- ----------------------------------------
--
-- set/modify collection entries

insertColByName :: ObjId -> ObjId -> Cmd ()
insertColByName i = adjustColByName [mkColColRef i]

insertColByDate :: ObjId -> ObjId -> Cmd ()
insertColByDate i = adjustColByDate [mkColColRef i]

adjustColByName :: [ColEntry] -> ObjId -> Cmd ()
adjustColByName = adjustColBy sortByName

adjustColByDate :: [ColEntry] -> ObjId -> Cmd ()
adjustColByDate = adjustColBy sortByDate

adjustColBy :: ([ColEntry] -> Cmd [ColEntry]) ->
               [ColEntry] ->
               ObjId -> Cmd ()
adjustColBy sortCol cs parent'i = do
  verbose $ "adjustColBy begin"
  cs'old <- getImgVals parent'i theColEntries
  verbose $ "adjustColBy" ++ show cs'old
  cs'new <- sortCol $ cs'old `mergeColEntries` cs
  verbose $ "adjustColBy" ++ show cs'new
  adjustColEntries (const cs'new) parent'i
  verbose $ "adjustColBy end"

-- ----------------------------------------

findFstTxtEntry :: ObjId -> Cmd (Maybe (Int, ColEntry))
findFstTxtEntry = findFstColEntry isTxtEntry
  where
    isTxtEntry (ImgRef i n) = do
      nd <- getImgVal i
      let ty = nd ^? theParts . isoImgPartsMap . ix n . theImgType
      return $ maybe False (== IMGtxt) ty

    isTxtEntry (ColRef _) =
      return False

-- take the 1. text entry in a collection
-- and set the collection blog entry to this value
-- rm indicates, whether the entry is removed from the collection

setColBlogToFstTxtEntry :: Bool -> ObjId -> Cmd ()
setColBlogToFstTxtEntry rm i = do
  fte <- findFstTxtEntry i
  maybe (return ()) setEntry fte
  where
    setEntry (pos, ir@(ImgRef j n)) = do
      trc $ unwords ["setColBlogToFstTxtEntry", show i, show pos, show ir]
      adjustColBlog (const $ Just (j, n)) i
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

mkColByPath :: (ObjId -> ObjId -> Cmd ()) -> (ObjId -> Cmd MetaData) -> Path -> Cmd ObjId
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
