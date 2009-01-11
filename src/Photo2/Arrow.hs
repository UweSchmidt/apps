module Photo2.Arrow
where

import           Control.Monad.Error            ( runErrorT )
import           Control.Parallel.Strategies

import           Data.List                      ( isPrefixOf
                                                , sortBy
                                                )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map                       as M
import qualified Data.Tree.NTree.TypeDefs       as NT

import           Photo2.ArchiveTypes
import           Photo2.Config
import           Photo2.ExifData
import           Photo2.FilePath
import           Photo2.ImageOperations

import           System.IO

import           Text.XML.HXT.Arrow
import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

-- ------------------------------------------------------------

type CmdArrow    a b =           IOStateArrow AppState a b
type PathArrow   a b = Path   -> CmdArrow  a b
type ConfigArrow a b = Config -> PathArrow a b

runCmd  :: CmdArrow a b -> AppState -> IO AppState
runCmd cmd s0
    = do
      (s1, _res) <- runIOSLA (setTraceLevel 0 >>> clear >>> cmd) (initialState s0) undefined
      return (xio_userState s1)

runCmd' :: CmdArrow a b -> ([b] -> IO ()) -> AppState -> IO AppState
runCmd' cmd out
    = runCmd (listA cmd >>> arrIO out)

arrIOE  :: (a -> IOE b) -> CmdArrow a b
arrIOE io
    = arrIO (runErrorT . io)
      >>>
      ( ( arrIO (\ s -> hPutStrLn stderr s) >>> none )
        |||
        this
      )

errMsg  :: String -> CmdArrow a b
errMsg  msg
    = ( arrIO0 $ hPutStrLn stderr msg )
      >>>
      none

infixr 1 />>>/

-- lift >>> to PathArrow level

(/>>>/)                 :: PathArrow b c -> PathArrow c d -> PathArrow b d
(/>>>/) f g             = \ p -> f p >>> g p


withDefaultRes          :: PathArrow b c -> c -> PathArrow b c
withDefaultRes f d      = \ p -> f p `withDefault` d

-- ------------------------------------------------------------

setField                :: Setter AppState b -> CmdArrow b b
setField sf             = perform ( ( this &&& getUserState )
                                    >>> arr (uncurry sf)
                                    >>> setUserState
                                  )

getField                :: Getter AppState b -> CmdArrow a b
getField gf             = getUserState >>^ gf

data SelArrow a b       = SA { get :: CmdArrow a b
                             , set :: CmdArrow b b
                             }

mkSelA                  :: Selector AppState b -> SelArrow a b
mkSelA (g, s)           = SA { get = getField g
                             , set = setField s
                             }

theAlbums               :: SelArrow a AlbumTree
theAlbums               = mkSelA $ selAlbums

theConfig               :: SelArrow a Config
theConfig               = mkSelA $ selConfig

theConfigAttrs          :: SelArrow a Attrs
theConfigAttrs          = mkSelA $ selConfigAttrs

theConfigAttr           :: Name -> SelArrow a Value
theConfigAttr k         = mkSelA $ selConfigAttr k

theConfigPicAttrs       :: SelArrow a PicAttrs
theConfigPicAttrs       = mkSelA $ selConfigPicAttrs

theStatus               :: SelArrow a Status
theStatus               = mkSelA $ selStatus

theArchiveName          :: SelArrow a Href
theArchiveName          = mkSelA $ selArchiveName

theConfigName           :: SelArrow a Href
theConfigName           = mkSelA $ selConfigName

theWd                   :: SelArrow a Path
theWd                   = mkSelA $ selWd

-- ------------------------------------------------------------

getConfig       :: (Config -> a) -> CmdArrow b a
getConfig cf    = get theConfig >>^ arr cf

-- ------------------------------------------------------------

updateNode'     :: ArrowTree a => (Pic -> Pic) -> a Pic Pic -> a AlbumTree AlbumTree
updateNode' setEdit update
    = ( ( setNode $< (getNode >>> update >>> arr setEdit) )
      )
      `orElse`
      this

updateNode      :: ArrowTree a => a Pic Pic -> a AlbumTree AlbumTree
updateNode      = updateNode' id

editNode        :: (ArrowTree a, ArrowNF a) => a Pic Pic -> a AlbumTree AlbumTree
editNode ea     = rnfA (updateNode' (change theEdited (const True)) ea)

editNode'       :: (ArrowTree a, ArrowNF a) => (Pic -> Pic) -> a AlbumTree AlbumTree
editNode'       = editNode . arr

changeEdited    :: ArrowTree a => (Bool -> Bool) -> a AlbumTree AlbumTree
changeEdited f  = updateNode' (change theEdited f) this

setEdited       :: ArrowTree a => a AlbumTree AlbumTree
setEdited       = changeEdited (const True)

clearEdited     :: ArrowTree a => a AlbumTree AlbumTree
clearEdited     = changeEdited (const False)

entryEdited'    :: CmdArrow AlbumTree AlbumTree
entryEdited'    = ( (get theConfig >>> isA (optON optForceStore)) `guards` this )
                  <+>
                  entryEdited

albumEdited'    :: CmdArrow AlbumTree AlbumTree
albumEdited'    = ( (get theConfig >>> isA (optON optForceStore)) `guards` this )
                  <+>
                  deep entryEdited

entryEdited     :: CmdArrow AlbumTree AlbumTree
entryEdited     = (getNode >>> isA picEdited) `guards` this

isEditedAlbum   :: CmdArrow AlbumTree AlbumTree
isEditedAlbum   = ( entryEdited <+> (getChildren >>> entryEdited) )
                  `guards`
                  this

-- ------------------------------------------------------------

setComp         :: SelArrow a b -> b -> CmdArrow a a
setComp c v     = perform $ constA v >>> set c

changeComp      :: SelArrow a b -> (b -> b) -> CmdArrow a a
changeComp c f  = perform $ get c >>> f ^>> set c

clear           :: CmdArrow a a
clear           = setComp theStatus (Running 0)

done            :: CmdArrow a a
done            = changeComp theStatus stopTr

start           :: CmdArrow a a
start           = changeComp theStatus startTr

failed          :: String -> CmdArrow a a
failed msg      = setComp theStatus (Exc msg)

setArchiveName  :: String -> CmdArrow a a
setArchiveName n = setComp theArchiveName n

statusOK        :: CmdArrow a Status
statusOK        = get theStatus >>> isA statusOk

traceStatus     :: String -> CmdArrow a a
traceStatus msg
    = perform $
      traceS $<< (get theStatus &&& get (theConfigAttr "debug"))
    where
    traceS st "1" = traceMsg 0 (replicate ((level st) + 2) ' ' ++ msg)
    traceS _  _   = this

    level (Running i)   = 2 * i
    level _             = 0

traceStatus'    :: String -> CmdArrow a a
traceStatus' msg
    = traceStatus ("  " ++ msg)

-- ------------------------------------------------------------

withStatusCheck :: String -> CmdArrow a b -> CmdArrow a b
withStatusCheck msg action
    = start
      >>>
      traceStatus ("start   : " ++ msg)
      >>>
      ( ( action
          >>>
          traceStatus ("done    : " ++ msg)
          >>>
          done
        )
        `orElse`
        ( traceStatus ("failed  : " ++ msg)
          >>>
          failed msg
          >>>
          none
        )
      )

whenStatusOK    :: String -> CmdArrow a b -> CmdArrow a b
whenStatusOK msg action
    = ( statusOK `guards` ( runAction' $< get theStatus ) )
      `orElse`
      ( traceStatus ("error   : " ++ msg)
        >>>
        none
      )
    where
    runAction' oldStatus
        = action
          >>>
          setComp theStatus oldStatus

runAction       :: String -> CmdArrow a b -> CmdArrow a b
runAction msg action
    = whenStatusOK msg (withStatusCheck msg action)

-- ------------------------------------------------------------

loadDocData     :: (NFData b) => PU b -> String -> CmdArrow a b
loadDocData p doc
    = readDocument [ (a_remove_whitespace, v_1)
                   , (a_validate, v_0)
                   , (a_tagsoup, v_0)
                     -- don't use tagsoup, it reads lasily and does not close input files, so they can't be written later on
                   ] doc
      >>>
      documentStatusOk
      >>>
      -- runAction ("unpickle document: " ++ doc)
      (rnfA (xunpickleVal p))
      -- >>>
      -- perform ( xpickleDocument p [ (a_indent, v_1) ] "" )   -- just for debug
      >>>
      traceStatus' ("loaded  : " ++ show doc)

loadArchive     :: String -> CmdArrow a Archive
loadArchive doc
    = runAction ("loading and unpickling archive: " ++ show doc)
      ( loadDocData xpArchive doc
        >>>
        perform ( archRootAlbum ^>> set theAlbums )
        >>>
        perform ( archConfRef ^>> set theConfigName )
        >>>
        setArchiveName doc
      )

loadConfig      :: String -> CmdArrow a Config
loadConfig doc
    = runAction ("loading and unpickling config: " ++ show doc)
      ( runInLocalURIContext (loadDocData xpConfig doc)
        >>>
        set theConfig
      )

loadArchiveAndConfig    :: String -> CmdArrow a (AlbumTree, Config)
loadArchiveAndConfig doc
    = loadArchive doc
      >>>
      ( arr archRootAlbum
        &&&
        ( loadConfig $< arr archConfRef )
      )

loadAlbum       :: String -> String -> CmdArrow a AlbumTree
loadAlbum base doc
    = -- runAction ("loading and unpickling entry: " ++ show doc)
      ( runInLocalURIContext ( constA base >>> changeBaseURI
                               >>>
                               loadDocData xpAlbumTree doc
                             )
      )

loadAlbums      :: PathArrow a AlbumTree
loadAlbums p
    = runAction ("check album loaded for " ++ showPath p) $
      changeAlbums (processTree (const this)) $ p

loadAndCheckAlbum       :: PathArrow a AlbumTree
loadAndCheckAlbum p
    = runAction ("check for entry loaded " ++ showPath p) $
      ( changeAlbums (processTree (const this))
        />>>/
        checkPath
      )
      $ p

loadChildAlbums :: PathArrow a AlbumTree
loadChildAlbums p
    = -- runAction ("check for child entries loaded " ++ showPath p) $
      (  changeAlbums (processTree (const this))
         />>>/
         changeAlbums (processTreeChildren (const this))
      ) p

loadAllAlbums   :: PathArrow a AlbumTree
loadAllAlbums p
    = runAction ("check for all entries loaded " ++ showPath p) $
      changeAlbums (processTreeSelfAndDesc (const this)) $ p

-- ------------------------------------------------------------

storeAll                :: String -> ConfigArrow AlbumTree AlbumTree
storeAll fmt c
    | isPicFmt          = storeAllChangedEntries
    | otherwise         = storeAllChangedAlbums
    where
    defaultFmt                  = getDefOpt "picture" "store-format" c
    fmt'        | null fmt      = defaultFmt
                | otherwise     = fmt
    isPicFmt                    = fmt' /= "album"

-- ------------------------------------------------------------

storeAllChangedEntries  :: PathArrow AlbumTree AlbumTree
storeAllChangedEntries
    = changeAlbums $
      processTreeDescAndSelfUC storeChangedEntries

storeChangedEntries     :: PathArrow AlbumTree AlbumTree
storeChangedEntries p
    = ( ( ( runAction ("storing all entries at: " ++ showPath p)
            ( storeEntry' $<<< ( (getNode >>^ isAl)
                                 &&&
                                 getConfig (albumPath p)
                                 &&&
                                 get theConfigName )
            )
            `when`
            entryEdited'                                                -- entry edited or store forced
          )
          >>>
          clearEdited                                                   -- clear edited marks
          >>>
          setTheRef' clearPic p
        )
        `orElse`
        this                                                            -- errors when writing the album
      )
      `when` ( isEntryLoaded
               >>>
               neg (getChildren >>> deep entryEdited)                   -- only albums already loaded are of interest
             )

setTheRef'              :: (Pic -> Pic) -> PathArrow AlbumTree AlbumTree
setTheRef' cp p         = setRef $< getConfig (albumPath p)
                          where
                          setRef doc = updateNode (arr $ change theRef (const doc) >>> cp)
                                       >>>
                                       setChildren []

storeEntry'     :: Bool -> FilePath -> FilePath -> CmdArrow AlbumTree AlbumTree
storeEntry' isAlb doc conf
    = perform mkdir
      >>>
      ( storeDocData xpAlbumTree rootName doc conf
        `guards`
        this
      )
    where
    mkdir = constA doc >>> arrIOE mkDirectoryPath
    rootName
        | isAlb         = "album"
        | otherwise     = "picture"

-- ------------------------------------------------------------
-- store the album and all subalbums adressed by a path and mark them as unloaded

storeAllChangedAlbums   :: PathArrow AlbumTree AlbumTree
storeAllChangedAlbums
    = changeAlbums $
      processTreeDescAndSelfUC storeChangedAlbums

storeChangedAlbums      :: PathArrow AlbumTree AlbumTree
storeChangedAlbums p
    = ( ( ( runAction ("storing all albums at: " ++ showPath p)
            ( ( storeEntry' $<<< ( (getNode >>^ isAl)
                                   &&&
                                   getConfig (albumPath p)
                                   &&&
                                   get theConfigName )
              )
              >>>
              clearEdited                       -- and clear edited mark
              >>>
              setTheRef' id p
            )
            `when`
            (isAlbum >>> albumEdited')          -- album edited or store forced
          )
        )
        `orElse` this                           -- errors when writing the album
      )
      `when` ( isEntryLoaded
               >>>
               neg (getChildren >>> getChildren >>> deep entryEdited)
             )

-- ------------------------------------------------------------

storeEntry      :: Bool -> FilePath -> FilePath -> CmdArrow AlbumTree AlbumTree
storeEntry isAlb doc conf
    = perform mkdir
      >>>
      ( storeDocData xpAlbumTree rootName doc conf
        `guards`
        updateNode ( arr $ change theRef (const doc) )
      )
    where
    mkdir = constA doc >>> arrIOE mkDirectoryPath
    rootName
        | isAlb         = "album"
        | otherwise     = "picture"

storeDocData    :: PU b -> String -> FilePath -> FilePath -> CmdArrow b XmlTree
storeDocData p rootName doc conf
    = {- runAction ("pickle document: " ++ doc) -} ( xpickleVal p ) 
      >>>
      runAction ("write document:  " ++ doc)
                ( addDoctypeDecl rootName "" (pathFromTo doc (dirPath conf </> "archive.dtd"))
                  >>>
                  perform (constA doc >>> arrIOE (mkBackupFile ".bak"))
                  >>>
                  writeDocument [ (a_indent, v_1)
                                , (a_output_encoding, isoLatin1)
                                ] doc
                )
      >>>
      documentStatusOk
      >>>
      traceStatus' ("stored  : " ++ show doc)

storeConfig     :: CmdArrow b XmlTree
storeConfig
    = storeC $< get theConfigName
    where
    storeC doc
        = runAction ("storing configuration: " ++ doc)
                    ( get theConfig
                      >>>
                      storeDocData xpConfig "config" doc doc
                    )

storeArchive    :: CmdArrow b XmlTree
storeArchive
    = storeA $<< (get theArchiveName &&& get theConfigName)
    where
    storeA doc confName
        = runAction ("storing the archive: " ++ doc)
          ( get theAlbums
            >>>
            withRootDir cutA
            >>>
            arr (Archive confName)
            >>>
            storeDocData xpArchive "archive" doc confName
          )
    cutA p
        = setChildren []
          >>>
          ( (\ doc -> updateNode (arr $ change theRef (const doc))) $< getConfig (albumPath p) )

-- ------------------------------------------------------------
--
-- check whether an album is loaded, if not yet done, load the album

checkEntryLoaded        :: CmdArrow AlbumTree AlbumTree
checkEntryLoaded
    = ( getExtEntryRef
        >>>
        ( loadAlbum $<< get theArchiveName &&& this )
      )
      `orElse` this

isEntryLoaded           :: CmdArrow AlbumTree AlbumTree
isEntryLoaded           = neg getExtEntryRef `guards` this

-- check whether an entry addresed by a path exists
-- in a tree

checkPath       :: PathArrow AlbumTree AlbumTree
checkPath p
    = runAction ("checking path: " ++ showPath p) $
      getTree p `guards` this

withLocalAlbums :: CmdArrow a b -> CmdArrow a b
withLocalAlbums ca
    = -- runAction ("with local copy of albums tree") $
      ( get theAlbums &&& this )        -- make a copy of the albums
      >>>
      ( this *** listA ca )             -- run the arrow as listA, because of possible failure
      >>>
      ( set theAlbums *** this )        -- restore the copy
      >>>
      arrL snd                          -- and return the arrow result

-- ------------------------------------------------------------
--
-- traversal functions for reading the tree

getTree :: PathArrow AlbumTree AlbumTree
getTree = getTreeAndProcess (const this)

getTreeAndProcess       :: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcess pa p0
    = getSub p0
    where
    getSub p
        | null p        = none
        | null p'       = nodeMatch `guards` (checkAndProcess pa p0)
        | otherwise     = nodeMatch `guards` (checkEntryLoaded >>> getChildren >>> getSub p')
        where
        (n' : p') = p
        nodeMatch = hasPicId n'

getTreeAndProcessChildren       :: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessChildren       = getTreeAndProcess . (getChildrenAndProcess getChildren)

getTreeAndProcessChildrenC      :: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessChildrenC      = getTreeAndProcess . (getChildrenAndProcess getChildrenAndCheck)

getTreeAndProcessDesc           :: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessDesc           = getTreeAndProcess . (getDescAndProcess getChildren)

getTreeAndProcessDescC          :: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessDescC          = getTreeAndProcess . (getDescAndProcess getChildrenAndCheck)

getTreeAndProcessSelfAndDesc    :: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessSelfAndDesc     = getTreeAndProcess . (getSelfAndDescAndProcess getChildren)

getTreeAndProcessSelfAndDescC   :: PathArrow AlbumTree b -> PathArrow AlbumTree b
getTreeAndProcessSelfAndDescC    = getTreeAndProcess . (getSelfAndDescAndProcess getChildrenAndCheck)

getChildrenAndProcess           :: CmdArrow AlbumTree AlbumTree ->
                                   PathArrow AlbumTree b -> PathArrow AlbumTree b
getChildrenAndProcess children pa p
    = children          -- optionally check album loaded
      >>>
      ( pa $< (getPicId >>^ (reverse . (:rp))) )
    where
    rp = reverse p      -- make p ++ [n] a bit more efficient

getDescAndProcess               :: CmdArrow AlbumTree AlbumTree ->
                                   PathArrow AlbumTree b -> PathArrow AlbumTree b
getDescAndProcess children pa p
    = getD (reverse p)  -- make p ++ [n] a bit more efficient
    where
    getD rp = children
              >>>
              ( (\ rp' -> pa (reverse rp') <+> getD rp') $< (getPicId >>^ (:rp)) )

getSelfAndDescAndProcess        :: CmdArrow AlbumTree AlbumTree ->
                                   PathArrow AlbumTree b -> PathArrow AlbumTree b
getSelfAndDescAndProcess children pa p
    = pa p
      <+>
      getDescAndProcess children pa p

getChildrenAndCheck             :: CmdArrow AlbumTree AlbumTree
getChildrenAndCheck             = getChildren >>> checkEntryLoaded

-- ----------------------------------------
--
-- | process all nodes of a tree

processAll      :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processAll pa p
    = pa p
      >>>
      processChildren ( (\ n -> processAll pa (p ++ [n])) $< getPicId )

-- ----------------------------------------
--
-- | process a tree addressed by a path
--   with an arrow getting the full path as parameter

processTree     :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTree pa p0
    = processSub p0
    where
    processSub p
        | null p        = this
        | null p'       = checkAndProcess pa p0
                          `when`
                          nodeMatch
        | otherwise     = ( checkEntryLoaded >>> processChildren (processSub p') )
                          `when`
                          nodeMatch
        where
        (n' : p') = p
        nodeMatch = hasPicId n'

loadProcessStore                :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
loadProcessStore pa p
    = runAction ("newformat for entry: " ++ showPath p) $
      choiceA
      [ isEntryLoaded   :-> pa p
      , this            :-> ( checkEntryLoaded
                              >>>
                              pa p
                              >>>
                              storeChangedEntries p
                            )
      ]

processTreeChildren             :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeChildren             = processTree . selChildrenAndProcess . checkAndProcess

processTreeDescTD               :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeDescTD               = processTree . selDescAndProcessTD . checkAndProcess

processTreeDescBU               :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeDescBU               = processTree . selDescAndProcessBU . checkAndProcess

processTreeSelfAndDesc          :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeSelfAndDesc          = processTree . selSelfAndDescAndProcessTD . checkAndProcess

processTreeDescAndSelfUC        :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeDescAndSelfUC        = processTree . selSelfAndDescAndProcessBU      -- don't load unloaded subalbums

processTreeDescAndSelf          :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeDescAndSelf          = processTree . selSelfAndDescAndProcessBU . checkAndProcess

checkAndProcess                 :: PathArrow AlbumTree b         -> PathArrow AlbumTree b
checkAndProcess pa p            = checkEntryLoaded >>> pa p

selChildrenAndProcess           :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selChildrenAndProcess pa p
    = processChildren
      ( pa $< (getPicId >>^ (reverse . (:rp))) )
    where
    rp = reverse p      -- make p ++ [n] a bit more efficient

selDescAndProcessTD             :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selDescAndProcessTD pa p
    = processD (reverse p)      -- make p ++ [n] a bit more efficient
    where
    processD rp = processChildren
                  ( (\ rp' -> pa (reverse rp') >>> processD rp') $< (getPicId >>^ (:rp)) )

selDescAndProcessBU             :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selDescAndProcessBU pa p
    = processD (reverse p)      -- make p ++ [n] a bit more efficient
    where
    processD rp = processChildren
                  ( (\ rp' -> processD rp' >>> pa (reverse rp')) $< (getPicId >>^ (:rp)) )

selSelfAndDescAndProcessTD      :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selSelfAndDescAndProcessTD pa p
    = pa p
      >>>
      selDescAndProcessTD pa p

selSelfAndDescAndProcessBU      :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selSelfAndDescAndProcessBU pa p
    = selDescAndProcessBU pa p
      >>>
      pa p

-- ------------------------------------------------------------

processTreeSelfAndDesc'         :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
processTreeSelfAndDesc'         = processTree . selSelfAndDescAndProcessTD'

selSelfAndDescAndProcessTD'     :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selSelfAndDescAndProcessTD' pa p
    = loadProcessStore pa p
      >>>
      selDescAndProcessTD' pa p

selDescAndProcessTD'            :: PathArrow AlbumTree AlbumTree -> PathArrow AlbumTree AlbumTree
selDescAndProcessTD' pa p
    = processD (reverse p)      -- make p ++ [n] a bit more efficient
    where
    processD rp = processChildren
                  ( (\ rp' -> loadProcessStore pa (reverse rp') >>> processD rp') $< (getPicId >>^ (:rp)) )

-- ----------------------------------------

hasPicId        :: ArrowTree a => Name -> a AlbumTree AlbumTree
hasPicId n      = (getPicId >>> isA (==n)) `guards` this

getPicId        :: ArrowTree a => a AlbumTree Name
getPicId        = getNode >>^ picId

mkPic           :: ArrowTree a => Pic -> a AlbumTree AlbumTree
mkPic           = mkLeaf

getRootPath     :: CmdArrow a Path
getRootPath     = get theAlbums >>> getPicId >>^ (:[])

rootWd          :: CmdArrow a Path
rootWd          = getRootPath >>> set theWd

withDir         :: Path -> PathArrow a b -> CmdArrow a b
withDir p c     = (\ p' -> withAbsDir p' c) $< (get theWd >>^ (++ p))

withAbsDir      :: Path -> PathArrow a b -> CmdArrow a b
withAbsDir p c  = c $< findAlbumPath (normalPath p)

withCwd         :: PathArrow a b -> CmdArrow a b
withCwd         = withDir []

withRootDir     :: PathArrow a b -> CmdArrow a b
withRootDir c   = (\ p' -> withAbsDir p' c) $< getRootPath

mkAbs           :: CmdArrow Path Path
mkAbs           = (\ wd -> arr (wd ++)) $< get theWd

withConfig      :: ConfigArrow a b -> PathArrow a b
withConfig c p  = ( \ conf -> c conf p ) $< get theConfig

changeAlbums    :: PathArrow AlbumTree AlbumTree -> PathArrow a AlbumTree
changeAlbums pa p
                = get theAlbums >>> pa p >>> set theAlbums

isAlbum         :: CmdArrow AlbumTree AlbumTree
isAlbum         = ( getNode >>> isA isAl ) `guards` this

getExtEntryRef  :: CmdArrow AlbumTree String
getExtEntryRef  = getNode >>> picRef ^>> isA (not . null)

-- ----------------------------------------

getRelatives    :: PathArrow AlbumTree (Path, Path, Path)
getRelatives p
    = -- runAction ("get relatives: " ++ showPath p) $
      getPN ( if null p then emptyPath else init p )
    where
    n = last p
    getPN pp
        = ( listA ( getTree pp
                    >>>
                    getChildren
                    >>>
                    getPicId
                  )
            >>>
            ( ( arrL getp `orElse` constA emptyPath )
              &&&
              ( arrL getn `orElse` constA emptyPath )
            )
            >>^ (\ (p', n') -> (pp, p',n'))
          )
          `orElse`
          constA (pp, emptyPath, emptyPath)
        where
        getn :: [Name] -> [Path]
        getn l = take 1 . map ((pp ++) . (:[]) . snd) . filter ((== n) . fst) . zip l . drop 1 $ l

        getp :: [Name] -> [Path]
        getp l = take 1 . map ((pp ++) . (:[]) . snd) . filter ((== n) . fst) . zip (drop 1 l) $ l

getAllWithAttr          :: String -> String -> PathArrow AlbumTree (Path, String, String)
getAllWithAttr rek rev
    = getTreeAndProcessSelfAndDescC $
      ( const ( getNode
                >>>
                arrL (load theAttrs >>> M.toList)
                >>>
                matchKA
                >>>
                matchKV
              )
        />>>/
        (\ p -> arr (\ (k, v) -> (p, show k, v)))
      )
    where
    matchK = match rek
    matchV = match rev
    matchKA = if null rek then this else isA (matchK . show . fst)
    matchKV = if null rev then this else isA (matchV        . snd)

-- ------------------------------------------------------------

addAlbumEntry   :: AlbumEntry -> CmdArrow AlbumTree AlbumTree
addAlbumEntry (p0, pic)
    = insert p0
    where
    n = picId pic
    insert      :: PathArrow AlbumTree AlbumTree
    insert p
        | null p        = setNode pic `when` hasPicId n                 -- entry already in tree
        | null p'       = replaceChildren (getChildren <+> mkPic pic)   -- append a new leave to the children
                          `when`
                          hasPicId n'
        | otherwise     = processChildren (insert p')                   -- descend into tree
                          `when`
                          hasPicId n'
        where
        (n'  : p' ) = p

removeAlbumEntry        :: PathArrow AlbumTree AlbumTree
removeAlbumEntry p
    = processTree (const none) p

-- ------------------------------------------------------------
-- update an attribute for an album or picture

updateAttrKeys  :: ConfigArrow AlbumTree AlbumTree
updateAttrKeys c p
    = runAction ("updating attribute keys for " ++ showPath p) $
      editNode' (change theAttrs (normAttrs (confPicAttrs c)))

updateAttr      :: String -> String -> ConfigArrow AlbumTree AlbumTree
updateAttr an av c p
    = runAction ("updating " ++ showPath p ++ " attr " ++ show an ++ " with value " ++ show av) $
      editNode' (change theAttrs (mergeAttr (newAttrKey (confPicAttrs c) an) av))

{-
updateAttrs     :: Attrs -> PathArrow AlbumTree AlbumTree
updateAttrs am p
    = runAction ("updating " ++ showPath p ++ " attributes " ++ show am) $
      editNode' (change theAttrs (mergeAttrs am))
-}
updateExifAttrs :: ConfigArrow AlbumTree AlbumTree
updateExifAttrs c p
    = runAction ("updating " ++ showPath p ++ " exif attributes") $
      editNode (arrIOE (importExifAttrs c p))

deleteAttr      :: String -> PathArrow AlbumTree AlbumTree
deleteAttr an p
    = runAction ("deleting " ++ showPath p ++ " attr " ++ show an) $
      editNode' (change theAttrs (remAttrs an))

deleteCopies    :: ConfigArrow AlbumTree AlbumTree
deleteCopies c p
    = runAction ("deleting image copies for " ++ showPath p) $
      ( perform ( getNode
                  >>>
                  arr (load theCopies >>> M.keys >>> map show)
                  >>>
                  arrIOE (rmCopies (joinPath p) (getImgType c))
                )
        >>>
        editNode' (change theCopies (const M.empty))
      )

-- ------------------------------------------------------------
--
-- new empty album

newAlbum        :: Name -> ConfigArrow AlbumTree AlbumTree
newAlbum nn _c p
    = runAction ("newalbum " ++ show nn ++ " in " ++ showPath p)
      ( checkEntryLoaded
        >>>
        ( ( notYetThere
            `guards`
            ( replaceChildren (getChildren <+> newEmptyAlbum)
              >>>
              editNode' id
            )
          )
          `orElse`
          errMsg ("album " ++ show nn ++ " already defined in " ++ showPath p)
        )
      )
    where
    notYetThere         = neg ( getChildren >>> getPicId >>> isA (== nn) )
    newEmptyAlbum       = arr . const . albumTree $ emptyPic { picId     = nn
                                                             , isAl      = True
                                                             , picEdited = True
                                                             }

setAlbumPic     :: Name -> ConfigArrow AlbumTree AlbumTree
setAlbumPic nn c p
    = runAction ("setalbumpic" ++ show nn ++ " for " ++ showPath p)
      ( checkEntryLoaded
        >>>
        processChildren checkEntryLoaded
        >>>
        ( ( ( ( setPic $< lookupPic )
              >>>
              deleteAttr "(cam|exif|file):.*" p
              >>>
              deleteCopies c p
            )
            `orElse`
            errMsg ("picture " ++ show nn ++ " not found in album " ++ showPath p)
          )
          `when` isAlbum
        )
      )
    where
    setPic pic
        = editNode' ( \ p' -> p' { picOrig   = picOrig pic
                                 , picRaw    = picRaw  pic
                                 , picXmp    = picXmp  pic
                                 }
                    )
    lookupPic
        = getChildren >>> getNode >>> isA ((== nn) . picId)

-- ------------------------------------------------------------
--
-- rename picture

renamePic       :: Name -> ConfigArrow AlbumTree AlbumTree
renamePic nn c p
    = runAction ("renaming " ++ showPath p ++ " to " ++ nn)
      ( checkEntryLoaded
        >>>
        editNode (arrIOE (mvPic nn c p))
      )

{-
renameContent   :: ConfigArrow AlbumTree AlbumTree
renameContent c p
    = runAction ("renaming album contents for " ++ showPath p)
      ( checkEntryLoaded
        >>>
        processChildren checkEntryLoaded
        >>>
        ( rename $< listA (getChildren >>> getPicId) )
        >>>
        setEdited
      )
    where
    rename      :: [Name] -> CmdArrow AlbumTree AlbumTree
    rename ids
        | null nameMap
            = this
        | otherwise             -- rename in 2 steps, to prevent name clashes
            = processChildren (renameChild nameMap1 $< getPicId)
              >>>
              processChildren (renameChild nameMap2 $< getPicId)
        where
        renameChild nm cid
            | isNothing nid     = this
            | otherwise         = renamePic (fromJust nid) c (p ++ [cid])
                                  `whenNot`
                                  isAlbum
            where
            nid = lookup cid nm

        nameMap         :: [(Name, Name)]
        nameMap         = filter isNewName (zip ids (map picnr [1..]))

        nameMap1        = map (\ (o,  n) -> (o, "#" ++ n)) $ nameMap
        nameMap2        = map (\ (_o, n) -> ("#" ++ n, n)) $ nameMap

        isNewName       :: (Name, Name) -> Bool
        isNewName (o, n)
            = n /= o
              &&
              (match "pic-[0-9]+" $ o)
-}
picnr :: Int -> String
picnr = show
        >>> reverse
        >>> (++ "0000")
        >>> take 4
        >>> reverse
        >>> ("pic-" ++)

-- ------------------------------------------------------------
--
-- sort all pictures by date

sortPics        :: ConfigArrow AlbumTree AlbumTree
sortPics _ p
    = runAction ("sorting " ++ showPath p ++ " by date")
      ( checkEntryLoaded
        >>>
        processChildren checkEntryLoaded
        >>>
        changeChildren sortByDate
        >>>
        setEdited
      )
      `when` isAlbum
    where
    sortByDate  = sortBy (\ t1 t2 -> compare (date t1) (date t2))
    date        = fromMaybe "" . M.lookup keyCreateDate . picAttrs . NT.getNode

-- ------------------------------------------------------------
--
-- update image data for a single node, the arrow input

updatePic       :: ConfigArrow AlbumTree AlbumTree
updatePic c p
    = runAction ("updating " ++ p')
      ( checkEntryLoaded
        >>>
        editNode update
      )
    where
    p' = showPath p
    sl = confSizes c
    update
        = runAction ("import original for " ++ p')
          ( arrIOE (importOrig c p) )
          >>>
          seqA (map copy sl)
    copy s
        = runAction ("create copy for " ++ show (sizeDir s) ++ " for " ++ p')
          ( arrIOE (createCopy c p s) )

-- ------------------------------------------------------------
--
-- cleanup image dirs

cleanupImgDirs  :: Bool -> Bool -> ConfigArrow AlbumTree ()
cleanupImgDirs execute rec conf p0
    = runAction ("cleanup image dirs for " ++ showPath p0) $
      ( checkEntryLoaded
        >>>
        perform ( cleanupDirs p0 $<
                  listA (findAlbumDir <+> findOrgDir <+> findImgDirs <+> findHtmlDirs)
                )
        >>>
        constA ()
      )
    where
    findAlbumDir
        = ( constA (getOpt "album-dir" $ conf) >>> isA (not . null) )
          &&&
          constA "xml"

    findOrgDir
        = ( constA (getOpt "dir"     $ conf) >>> isA (not . null) )
          &&&
          constA (getImgType conf)

    findImgDirs
        = ( constL (confSizes conf) >>^ sizeDir )
          &&&
          constA (getImgType conf)

    findHtmlDirs
        = ( constL (map (show . fst) . filter isHtmlFormat . M.assocs . confLayouts $ conf) )
          &&&
          constA "html"
        where
        isHtmlFormat = ("html" `isPrefixOf`) . layoutType . snd  

    cleanupDirs :: Path -> [(String,String)] -> CmdArrow AlbumTree AlbumTree
    cleanupDirs path dirsExts
        | rec           = getSelfAndDescAndProcess getChildrenAndCheck cleanup1Dir path
        | otherwise     =                                              cleanup1Dir path
        where
        cleanup1Dir p'  = seqA . map (cleanup1Dir' p') $ dirsExts

        cleanup1Dir' p' (dir, ext)
            = perform
              ( runAction ("cleanup " ++ show dir' ++ " with " ++ show ext ++ " files")
                ( listA (getChildren >>> getNode >>^ picId)
                  >>>
                  arrIOE (cleanupDir execute dir' ext)
                )
              )
              `when` isAlbum
            where
            dir'                = dir </> joinPath p'

-- ------------------------------------------------------------

findAlbumPath	:: PathArrow a Path
findAlbumPath p
    = get theAlbums
      >>>
      findPath' p
      >>>
      isA (not . null . fst)
      >>>
      perform (snd ^>> set theAlbums)
      >>>
      arr fst

findPath'	:: PathArrow AlbumTree (Path, AlbumTree)
findPath' p	= -- runAction ("findpath for " ++ showPath p) $
		  findPath p

findPath	:: PathArrow AlbumTree (Path, AlbumTree)
findPath p
    | null p	= none
    | null p'	= ( ifA nodeMatch
		       (getPicId >>^ return)
                       (constA [])
		  )
		  &&&
		  this
    | otherwise	= ifA nodeMatch
		      ( checkEntryLoaded
			>>>
			( replaceCh $< findCh p')
		      )
		      (constA [] &&& this)
    where
    (n' : p') = p
    nodeMatch = getPicId >>> isA (wildcardMatch n')

    replaceCh (newp', newCh)
	| null newp'	= none
	| otherwise	= (getPicId >>^ (:newp'))
			  &&&
			  replaceChildren (constL newCh)

    findCh	:: PathArrow AlbumTree (Path, [AlbumTree])
    findCh p''	= -- runAction ("findCh with path " ++ showPath p'') $
		  listA ( getChildren
			  >>>
			  findPath' p''
			)
                  -- >>>
		  -- perform ( arr (map fst) >>> arrIO print)
		  >>^
		  splitPathsTrees
		  where
		  splitPathsTrees ps
		      | null np				-- no path found
			||
			not (null (tail np))		-- 2 or more paths found: ambigious path
			  = ([],[])
		      | otherwise
			  = (head np, map snd ps)
		      where
		      np = filter (not . null) . map fst $ ps

wildcardMatch	:: String -> String -> Bool
wildcardMatch p
    | containsWCs p	= match p'
    | otherwise		= (p ==)
    where
    containsWCs	= any (`elem` "*?[]")
    p'		= concatMap substWC p

    substWC '*'	= ".*"
    substWC '?' = "."
    substWC c   = [c]

-- ------------------------------------------------------------
