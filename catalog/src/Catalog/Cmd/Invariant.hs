module Catalog.Cmd.Invariant
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Data.ImageStore
import           Data.ImgTree
import           Data.Prim

import qualified Data.Set as S
import qualified Data.Map.Strict as M

-- ----------------------------------------

checkImgStore :: Cmd ()
checkImgStore = do
  verbose "checkImgStore: check integrity of the archive"
  allCleanupImgRefs
  cleanupDeadRefs
  _us <- allUndefRefs
  _ps <- allCheckUpLink
  return ()

-- ----------------------------------------

allCleanupImgRefs :: Cmd ()
allCleanupImgRefs = getRootId >>= cleanupImgRefs

cleanupImgRefs :: ObjId -> Cmd ()
cleanupImgRefs i0 = do
  p <- objid2path i0
  verbose $ "cleanImgRefs: remove outdated img refs for: " ++ show (i0, p)

  foldMT' undefId imgA dirA rootA colA i0
  where
    undefId i
      = warn $ "cleanImgRefs: undefined obj id ignored: " ++ show i

    imgA _i _pts _md
      = return ()

    dirA go i es _ = do
      p <- objid2path i
      verbose $ "cleanImgRefs: process img dir: " ++ quotePath p
      verbose "cleanImgRefs: not yet implemented"
{-
      es' <- filterM (isOK checkImgRef) (es ^. isoDirEntries)
      when (length es' < length (es ^. isoDirEntries)) $ do
        warn $ "cleanImgRefs: col entries removed in: "
               ++ quotePath p ++ ", " ++ show (es, es')
        adjustDirEntries (const $ es' ^. from isoDirEntries) i

      mapM_ go es'
-}
    colA go i _md im be es = do
      p <- objid2path i
      trc $ "cleanImgRefs: process collection: " ++ quotePath p

      im' <- filterMM (isOK (uncurry checkImgPart)) im
      when (im /= im') $ do
        warn $ "cleanImgRefs: col img ref removed in: "
               ++ quotePath p ++ ", " ++ show (im, im')
        adjustColImg (const im') i

      be' <- filterMM (isOK (uncurry checkImgPart)) be
      when (be /= be') $ do
        warn $ "cleanImgRefs: col blog ref removed in: "
               ++ quotePath p ++ ", " ++ show (be, be')
        adjustColBlog (const be') i

      es' <- filterM (isOK checkColEntry) es
      when (length es' < length es) $ do
        warn $ "cleanImgRefs: col enties removed in: "
               ++ quotePath p ++ ", " ++ show (es, es')
        adjustColEntries (const es') i

      mapM_ (colEntry (\ _ _ -> return ()) go) es'

    rootA go _i dir col = go dir >> go col

    filterMM :: (a -> Cmd Bool) -> Maybe a -> Cmd (Maybe a)
    filterMM _ Nothing = return Nothing
    filterMM f r@(Just x) = do
      ok <- f x
      return $
        if ok then r else Nothing

    isOK :: (b -> Cmd (Maybe a)) -> b -> Cmd Bool
    isOK cmd i = isJust <$> cmd i

    -- check whether the ref i exists and points to an IMG value
    checkImgRef :: ObjId -> Cmd (Maybe ImgNode)
    checkImgRef i = do
      mn <- getTree (entryAt i)
      return $
        case mn of
          Nothing -> Nothing
          Just r ->
            let n = r ^. nodeVal in
            if isIMG n
            then Just n
            else Nothing

    -- check whether both the ref and the part in an ImgRef exist
    checkImgPart :: ObjId -> Name -> Cmd (Maybe ImgNode)
    checkImgPart i nm = do
      mn <- checkImgRef i
      return $
        case mn of
          Nothing -> Nothing
          Just n ->
            case n ^? theParts . isoImgPartsMap . at nm of
              Nothing -> Nothing
              Just _  -> mn

    checkColEntry :: ColEntry -> Cmd (Maybe ColEntry)
    checkColEntry ce = colEntry imgRef (const $ return $ Just ce) ce
      where
        imgRef i nm = do res <- checkImgPart i nm
                         return (const ce <$> res)

-- ----------------------------------------

allUndefRefs :: Cmd (Set ObjId)
allUndefRefs = getRootId >>= undefRefs

undefRefs :: ObjId -> Cmd (Set ObjId)
undefRefs i0 = do
  p <- objid2path i0
  verbose $ "undefRefs: search undefined refs for: " ++ show (i0, p)
  s <- foldMT' undefId imgA dirA rootA colA i0
  verbose $ "undefRefs: to be cleaned up: " ++ show (S.toList s)
  return s
  where
    undefId i
      = do warn $ "undefRefs: undefined obj id found: " ++ show i
           return $ S.singleton i

    imgA _i _pts _md
      = return S.empty

    dirA go i es _ = do
      s <- S.unions <$> mapM go (es ^. isoDirEntries)
      warnU i s

    colA go i _md im be es = do
      s1 <- mapMb im
      s2 <- mapMb be
      s3 <- mapM (go . (^. theColObjId)) (filter isColColRef es)
      warnU i $ S.unions (s1 : s2 : s3)
      where
        mapMb =
          maybe (return S.empty) (go . fst)

    rootA go i dir col = do
      s <- S.union <$> go dir <*> go col
      warnU i s

    warnU i s
      | S.null s =
          return s
      | otherwise = do
          p <- objid2path i
          warn $ "undefRefs: undefined refs found: " ++ show (p, S.toList s)
          return s

-- ----------------------------------------

allDefinedRefs :: Cmd (Set ObjId)
allDefinedRefs = getRootId >>= definedRefs

definedRefs :: ObjId -> Cmd (Set ObjId)
definedRefs i0 = do
  p <- objid2path i0
  verbose $ "definedRefs: compute defined refs for: " ++ show (i0, p)
  s <- foldMT' undefId imgA dirA rootA colA i0
  verbose $ "definedRefs: refs found: " ++ show (S.size s)
  return s
  where
    undefId i = do
      warn $ "definedRefs: undefined obj id found: " ++ show i
      return S.empty

    imgA i _pts _md =
      return $ S.singleton i

    dirA go i es _ = do
      (S.insert i . S.unions) <$> mapM go (es ^. isoDirEntries)

    colA go i _md im be es = do
      s1 <- maybe (return S.empty) (go . fst) im
      s2 <- maybe (return S.empty) (go . fst) be
      s3 <- mapM (go . (^. theColObjId)) (filter isColColRef es)
      return $ S.insert i $ S.unions (s1 : s2 : s3)

    rootA go i dir col = do
      s <- S.union <$> go dir <*> go col
      return $ S.insert i s

-- ----------------------------------------

allDeadRefs :: Cmd (Set ObjId)
allDeadRefs = do
  us <- allDefinedRefs
  as <- (S.fromList . M.keys) <$> getTree entries
  return $ as `S.difference` us

cleanupDeadRefs :: Cmd ()
cleanupDeadRefs = do
  ds <- allDeadRefs
  unless (S.null ds) $ do
    warn $ "cleanupDeadRefs: removing read refs: " ++ show (S.toList ds)
    theImgTree . entries %= rmDeadRefs ds
  where
    rmDeadRefs ds m = m `M.difference` M.fromSet (const ()) ds

-- ----------------------------------------

allCheckUpLink :: Cmd (Set ObjId)
allCheckUpLink = getRootId >>= checkUpLink

checkUpLink :: ObjId -> Cmd (Set ObjId)
checkUpLink i0 = do
  p <- objid2path i0
  verbose $ "checkUpLink: check uplinks for: " ++ show (i0, p)
  s <- foldMT' undefId imgA dirA rootA colA i0
  unless (S.null s) $
    warn $ "checkUpLink: wrong uplinks found: " ++ show (S.toList s)
  return s
  where
    undefId i
      = do warn $ "checkUpLink: undefined obj id found: " ++ show i
           return S.empty

    imgA _i _pts _md =
      return S.empty

    dirA go i es _ = do
      ps <- filter (/= i) <$> mapM getImgParent (es ^. isoDirEntries)
      unless (null ps) $ do
        p <- objid2path i
        warn $ "checkUpLink: uplink(s) wrong in DIR node: " ++ show (p, ps)
      s1 <- mapM go (es ^. isoDirEntries)
      return $ S.fromList ps `S.union` S.unions s1

    colA go i _md _im _be es = do
      ps <- filter (/= i) <$> mapM getImgParent (es ^.. traverse . theColColRef)
      unless (null ps) $ do
        p <- objid2path i
        warn $ "checkUpLink: uplink(s) wrong in COL node: " ++ show (p, ps)
      s1 <- mapM (go . (^. theColObjId)) (filter isColColRef es)
      return $ S.fromList ps `S.union` S.unions s1

    rootA go i dir col = do
      s0 <- (filter (/= i) . (:[])) <$> getImgParent i
      unless (null s0) $ do
        warn $ "checkUpLink: uplink for root node wrong: " ++ show i
      s1 <- go dir
      s2 <- go col
      return $ S.fromList s0 `S.union` (s1 `S.union` s2)

-- ----------------------------------------

{-
listNames :: ObjId -> Cmd String
listNames i0 =
  unlines <$> foldMT imgA dirA rootA colA i0
  where
    nm i     = show <$> getImgName i
    ind n xs = n : map ("  " ++) xs

    imgA i ps _md = do
      n <- nm i
      return $
        ind n (ps ^.. isoImgParts . traverse . theImgName . isoString)

    dirA go i es _ts = do
      n  <- nm i
      xs <- mapM go (es ^. isoDirEntries)
      return $
        ind n (concat xs)

    rootA go i dir col = do
      n   <- nm i
      dns <- go dir
      cns <- go col
      return $
        ind n (dns ++ cns)

    colA go i _md _im _be es = do
      n   <- nm i
      cns <- mapM go' es
      return $
        ind n (concat cns)
      where
        go' (ImgRef _i n) =
          return [n ^. isoString]
        go' (ColRef i') =
          go i'

listPaths' :: ObjId -> Cmd [Path]
listPaths' =
  foldMT imgA dirA rootA colA
  where
    imgA i ps _md = do
      p  <- objid2path i
      let pp = ps ^.. isoImgParts . traverse . theImgName . to (`substPathName` p)
      return $
        p : pp

    dirA go i es _ts = do
      p  <- objid2path i
      pp <- mapM go (es ^. isoDirEntries)
      return $
        p : concat pp

    rootA go i dir col = do
      p  <- objid2path i
      pd <- go dir
      pc <- go col
      return $
        p : pd ++ pc

    colA go i _md _im _be es = do
      p  <- objid2path i
      pp <- mapM go' es
      return $
        p : concat pp
      where
        go' :: ColEntry -> Cmd [Path]
        go' (ImgRef i' n') = do
          ip <- objid2path i'
          return [substPathName n' ip]
        go' (ColRef i') =
          go i'

listPaths :: ObjId -> Cmd String
listPaths i = (unlines . map show) <$> listPaths' i

listImages' :: Cmd [(Path, [Name])]
listImages' = do
  r <- use (theImgTree . rootRef)
  foldImages listImg r
  where
    listImg :: ObjId -> ImgParts -> MetaData -> Cmd [(Path, [Name])]
    listImg i ps _md = do
      p <- objid2path i
      let pns = ps ^.. isoImgParts . traverse . theImgName
      return [(p, pns)]

listImages :: Cmd String
listImages = formatImages <$> listImages'
  where
    formatImages :: [(Path, [Name])] -> String
    formatImages = unlines . map (uncurry fmt)
      where
        fmt p ns = show p ++ ": " ++ intercalate ", " (map show ns)
-}
-- ----------------------------------------
