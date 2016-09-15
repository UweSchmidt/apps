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
genClipboardCollection = genSysCollection n'clipboard tt'clipboard

genTrashCollection :: Cmd ()
genTrashCollection = genSysCollection n'trash tt'trash

genSysCollection :: Name -> Text -> Cmd ()
genSysCollection n'sys tt'sys = do
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
        a = no'delete


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
      es' <- if colSyncTime >= dirSyncTime
             then
               getImgSubDirs es
             else
               return (es ^. isoDirEntries)
      concat <$> mapM go es'

    -- read the jpg image part and the create date meta tag
    -- and build a list of pairs of date and ColImgRef's
    imgA :: ObjId -> ImgParts -> Cmd [((String, String, String), ColEntry)]
    imgA i pts = do
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
        pm = py `snocPath` mkName m
        pd = pm `snocPath` mkName d

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

genCollectionsByDir :: Cmd ()
genCollectionsByDir = do
  ic <- getRootImgColId                 -- the collection root
  pc <- objid2path ic
  di <- getRootImgDirId
  dp <- objid2path di

  -- create root collection for archive dir hierachy
  let (rootName, pc1) = pc  ^. viewTop
  let (colName, _pc2) = pc1 ^. viewTop
  let old'px  = mkPath rootName
  let new'px  = rootName `consPath` mkPath colName
  let img2col = substPathPrefix old'px new'px

  void $ mkColByPath insertColByName setupDirCol (img2col dp)

  void $ genCol img2col di
  -- adjustColEntries (`mergeColEntries` es) ic
  -- trc $ "genCollectionsbydir: " ++ show es
  -- return ()
  where
    -- meta data for generated collections
    setupDirCol :: ObjId -> Cmd MetaData
    setupDirCol i = do
      -- remove the 2 top level dirs
      -- remove leading "/"

      t <- path2Title <$> objid2path i
      let o = to'colandname
          a = no'wrtdel
      mkColMeta t "" "" o a

    -- TODO
    -- search for a IMGtxt entry in DIR entries, take the 1. as blog entry for the collection
    -- setupDirTxt :: ObjId -> Cmd (Maybe (ObjId, Name))
    -- setupDirTxt = undefined

    path2Title :: Path -> Text
    path2Title p = tt p ^. isoText
      where
        tt = pathToBreadCrump . show . tailPath . tailPath
        -- substitute / by ->

    genCol :: (Path -> Path) -> ObjId -> Cmd [ColEntry]
    genCol fp =
      processImgDirs imgA dirA
      where
        -- collect all processed jpg images for a single img

        imgA i pts = do
          let res = (map (mkColImgRef i) $ sort ns)
          -- trcObj i $ "genCol img: " ++ show res
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
          -- trcObj i $ "genCol dir " ++ show cp

          -- check or create colection
          -- with action for meta data
          ic <- mkColByPath insertColByName setupDirCol cp

          dirSyncTime <- getImgVals i  theSyncTime
          colSyncTime <- getImgVals ic theSyncTime

          if colSyncTime >= dirSyncTime
            then do
              -- the collection is up to date
              -- only the subdirs need to be traversed
              trcObj i "genCol dir: dir is up to date, traversing subdirs"
              cs  <- getImgSubDirs es
              void $ mapM go cs
            else do
              -- get collection entries, and insert them into collection
              cs  <- concat <$> mapM go (es ^. isoDirEntries)
              adjustColByName cs ic

              -- set the blog entry, if there's a txt entry in cs
              setColBlogToFstTxtEntry False ic
              
              -- set time processed
              setSyncTime ic

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
    getVal (ColRef j)       = Left                    <$> getImgName j
    getVal (ImgRef j n1 _m) = (\ n -> Right (n, n1))  <$> getImgName j

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
    getVal (ImgRef j n1 _m) = do
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
  cs'old <- getImgVals parent'i theColEntries
  cs'new <- sortCol $ cs'old `mergeColEntries` cs
  adjustColEntries (const cs'new) parent'i

-- ----------------------------------------

findFstTxtEntry :: ObjId -> Cmd (Maybe (Int, ColEntry))
findFstTxtEntry = findFstColEntry isTxtEntry
  where
    isTxtEntry (ImgRef i n _m) = do
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
    setEntry (pos, ir@(ImgRef j n _m)) = do
      trc $ unwords ["setColBlogToFstTxtEntry", show i, show pos, show ir]
      adjustColBlog (const $ Just (j, n)) i
      when rm $
        delColEntry pos i
    setEntry _ =
      return ()
    
-- ----------------------------------------

mkColMeta :: Text -> Text -> Text -> Text -> Text -> Cmd MetaData
mkColMeta t s c o a = do
  d <- (\ t' -> show t' ^. isoText) <$> atThisMoment
  return $
      mempty
      & metaDataAt "descr:Title"      .~ t
      & metaDataAt "descr:Subtitle"   .~ s
      & metaDataAt "descr:Comment"    .~ c
      & metaDataAt "descr:OrderedBy"  .~ o
      & metaDataAt "descr:CreateDate" .~ d
      & metaDataAt "descr:Access"     .~ a

-- create collections recursively, similar to 'mkdir -p'

mkColByPath :: (ObjId -> ObjId -> Cmd ()) -> (ObjId -> Cmd MetaData) -> Path -> Cmd ObjId
mkColByPath insertCol setupCol p = do
  -- trc $ "mkColByPath " ++ show p
  -- check for legal path
  when (isempty $ tailPath p) $
    abort $ "mkColByPath: can't create collection " ++ show (show p)

  mid <- lookupByPath p
  case mid of
    -- collection does not yet exist
    Nothing -> do
      -- compute parent collection
      let (p1, n) = p ^. viewBase
      ip <- mkColByPath insertCol setupCol p1
      -- trc $ "mkColByPath " ++ show p1 ++ "/" ++ show n

      -- create collection and set meta data
      ic <- mkImgCol ip n
      md <- setupCol ic
      adjustMetaData (md <>) ic

      -- inser collection into parent collection
      insertCol ic ip

      return ic

    -- entry already there
    Just (ip, vp) -> do
      unless (isCOL vp) $
        abort $ "mkColByPath: can't create collection, other entry already there " ++
                show (show p)
      return ip

-- ----------------------------------------
