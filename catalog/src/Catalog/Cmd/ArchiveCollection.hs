{-# LANGUAGE OverloadedStrings #-}

module Catalog.Cmd.ArchiveCollection
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Catalog.System.ExifTool
import           Catalog.System.IO
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------

-- TODO: use time stamp of dirs and collections to
-- skip unchanged dirs, similar to genCollectionsByDir

genCollectionsByDate :: Cmd ()
genCollectionsByDate = do
  ic <- getRootImgColId
  pc <- objid2path ic
  di <- getRootImgDirId

  -- create root col for year/month/day hierachy
  -- "/archive/collections/byCreateDate"

  let top'iPath = pc `snocPath` "byCreateDate"
  top'i <- mkColByPath insertColByName setupByDate top'iPath
  top'iSyncTime <- getImgVals top'i theColSyncTime

  -- fill the year/month/day hierachy
  processNewImages top'iSyncTime top'iPath di

  setSyncTime top'i
  where

    setupByDate :: ObjId -> Cmd MetaData
    setupByDate _i = do
      mkColMeta t s c o a
      where
        t = "Bilder geordnent nach Datum"
        s = ""
        c = ""
        o = "ColAndName"
        a = "ReadOnly"

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
          let res = [(ymd, mkColImgRef i n) | n <- ns]
          trcObj i $ "processnewimages res: " ++ show res
          return [(ymd, mkColImgRef i n) | n <- ns]
      where
        ns = pts ^.. thePartNames IMGjpg

    -- add all images for 1 day into the corresponding collection
    addToCol :: [((String, String, String), ColEntry)] -> Cmd ()
    addToCol dcs = do
      let ymd = fst . head $ dcs
          cs  = map snd dcs
      trc $ "addToCol " ++ show dcs
      -- check or create y/m/d hierachy
      (yc, mc, dc) <-mkDateCol ymd
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

        month :: Int -> String
        month i = [ "Januar", "Februar", "März", "April", "Mai", "Juni"
                  , "Juli", "August", "September","Oktober", "November", "Dezember"
                  ] !! (i - 1)

        setupYearCol y' _i = do
          mkColMeta t "" "" o a
          where
            t = ("Bilder aus " ++ y') ^. from isoString
            o = "Name"
            a = "ReadOnly"

        setupMonthCol y' m' _i = do
          mkColMeta t "" "" o a
          where
            t = unwords [ "Bilder aus dem"
                        , month (read m')
                        , y'
                        ]
                ^. from isoString
            o = "Name"
            a = "ReadOnly"

        setupDayCol y' m' d' _i = do
          mkColMeta t "" "" o a
          where
            t = unwords [ "Bilder vom"
                        , show (read d' :: Int) ++ "."
                        , month (read m')
                        , y'
                        ]
                ^. from isoString
            o = "DateAndTime"
            a = "ReadOnly"

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
      p <- (show . tailPath) <$> objid2path i
      let t = p ^. from isoString
          o = "ColAndName"
          a = "ReadOnly"
      mkColMeta t "" "" o a

    genCol :: (Path -> Path) -> ObjId -> Cmd [ColEntry]
    genCol fp =
      processImgDirs imgA dirA
      where
        -- collect all processed jpg images for a single img

        imgA i pts = do
          let res = (map (mkColImgRef i) $ sort ns)
          trcObj i $ "genCol img: " ++ show res
          return res
          where
            ns = pts ^.. thePartNames IMGjpg

        -- generate a coresponding collection with all entries
        -- entries are sorted by name

        dirA :: (ObjId -> Cmd [ColEntry]) ->
                ObjId -> DirEntries -> TimeStamp -> Cmd [ColEntry]
        dirA go i es _ts = do
          p  <- objid2path i
          let cp = fp p
          trcObj i $ "genCol dir " ++ show cp

          -- check or create colection
          -- with action for meta data
          ic <- mkColByPath insertColByName setupDirCol cp

          dirSyncTime <- getImgVals i  theDirSyncTime
          colSyncTime <- getImgVals ic theColSyncTime

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

              -- set time processed
              setSyncTime ic

          return [mkColColRef ic]

-- ----------------------------------------
--
-- collection sort

sortByName :: [ColEntry] -> Cmd [ColEntry]
sortByName =
  sortColEntries' getVal compare
  where

    -- collections come first and are sorted by name
    -- images are sorted by name and part name
    getVal :: ColEntry -> Cmd (Either Name (Name, Name))
    getVal (ColRef j)    = Left                    <$> getImgName j
    getVal (ImgRef j n1) = (\ n -> Right (n, n1))  <$> getImgName j

sortByDate :: [ColEntry] -> Cmd [ColEntry]
sortByDate =
  sortColEntries' getVal compare
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


sortColEntries' :: (ColEntry -> Cmd a) ->
                  (a -> a -> Ordering) ->
                  [ColEntry] -> Cmd [ColEntry]
sortColEntries' getVal cmpVal es = do
  map fst . sortBy (cmpVal `on` snd) <$> mapM mkC es
  where
    -- mkC :: ColEntry -> Cmd (ColEntry, a)
    mkC ce = do
      v <- getVal ce
      return (ce, v)

-- ----------------------------------------
--
-- merge old an new entries
-- old entries are removed from list of new entries
-- the remaining new entries are appended

mergeColEntries :: [ColEntry] -> [ColEntry] -> [ColEntry]
mergeColEntries es1 es2 =
  es1 ++ filter (`notElem` es1) es2

-- a faster version, where the result is unordered
-- duplicates are removed, useful when the list of entries
-- is sorted afterwards
mergeColEntries' :: [ColEntry] -> [ColEntry] -> [ColEntry]
mergeColEntries' ns os =
  (ns ++ os) ^. (from isoDirEntries) . isoDirEntries

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

mkColMeta :: Text -> Text -> Text -> Text -> Text -> Cmd MetaData
mkColMeta t s c o a = do
  d <- (\ t' -> show t' ^. from isoString) <$> atThisMoment
  return $
      mempty
      & metaDataAt "COL:Title"      .~ t
      & metaDataAt "COL:Subtitle"   .~ s
      & metaDataAt "COL:Comment"    .~ c
      & metaDataAt "COL:OrderedBy"  .~ o
      & metaDataAt "COL:CreateDate" .~ d
      & metaDataAt "COL:Access"     .~ a

-- create collections recursively, similar to 'mkdir -p'

mkColByPath :: (ObjId -> ObjId -> Cmd ()) -> (ObjId -> Cmd MetaData) -> Path -> Cmd ObjId
mkColByPath insertCol setupCol p = do
  trc $ "mkColByPath " ++ show p
  -- check for legal path
  when (isempty $ tailPath p) $
    abort $ "mkColByPath: can't create collection " ++show (show p)

  mid <- lookupByPath p
  case mid of
    -- collection does not yet exist
    Nothing -> do
      -- compute parent collection
      let (p1, n) = p ^. viewBase
      ip <- mkColByPath insertCol setupCol p1
      trcObj ip $ "mkColByPath " ++ show p1 ++ " " ++ show n

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
