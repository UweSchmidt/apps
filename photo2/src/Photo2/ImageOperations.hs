module Photo2.ImageOperations
where

import           Control.Arrow
import qualified Control.Exception           as CE

import           Control.Monad.Except        hiding (liftIO)
import qualified Control.Monad.Except        as ME

import           Control.DeepSeq             (rnf)


import           Data.Atom
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C
import           Data.List
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Time

import           Photo2.ArchiveTypes
import           Photo2.Config
import           Photo2.ExifData
import           Photo2.FilePath

import           System.Process              (system)
import           System.Directory
import           System.Exit
import           System.IO
import           System.Posix                (createLink, getProcessID)
{-
import           System.Time    ( ClockTime
                                , toCalendarTime
                                , formatCalendarTime
                                , getClockTime
                                )
-- -}
import           System.Locale               (defaultTimeLocale)

import           Text.Regex.XMLSchema.String (match, matchSubex, sed)

-- ------------------------------------------------------------

type IOE a      = ExceptT String IO a

-- ------------------------------------------------------------

-- lift IO commands to IOE commands and map IO errors to ErrorT errors

liftIO  :: IO a -> IOE a
liftIO a
    = do
      r <- ME.liftIO $
           catch' ( do
                    r1 <- a
                    return (Right r1)
                  )
                  (\ err -> return (Left $ show err))
      evalRc r
    where
    evalRc (Left msg)   = throwError msg
    evalRc (Right res)  = return res

catch' :: IO a -> (CE.SomeException -> IO a) -> IO a
catch' = CE.catch

mapError        :: IOE a -> (String -> String) -> IOE a
mapError a f
    = a `catchError` (throwError . f)

dryCmd  :: Bool -> String -> IOE () -> IOE ()
dryCmd True   msg  _cmd = liftIO $ hPutStrLn stderr ("dry run: " ++ msg)
dryCmd False _msg   cmd = cmd

-- ------------------------------------------------------------

mergeAttrs              :: Attrs -> Attrs -> Attrs
mergeAttrs n o          = M.foldrWithKey mergeAttr o n

mergeAttr               :: Atom -> Value -> Attrs -> Attrs
mergeAttr k v
    | v == "-"          = M.delete k                    -- "-" indicates delete attribute
    | k == keyKeywords  = mergeKeywords (words v)
    | k == keyGoogleMaps= mergeGeoTags v
    | null v            = M.delete k
    | otherwise         = M.insert k v

remAttrs                :: String -> Attrs -> Attrs
remAttrs kp             = M.foldrWithKey remK M.empty
    where
    remK k a m
        | match kp (show k)       = m
        | otherwise               = M.insert k a m

mergeKeywords           :: [String] -> Attrs -> Attrs
mergeKeywords ws a
    | null nws          = M.delete keyKeywords               a
    | otherwise         = M.insert keyKeywords (unwords nws) a
    where
    ows                 = nub . sort . words . fromMaybe "" . M.lookup keyKeywords $ a
    nws                 = foldl insertKeyw ows ws
    insertKeyw ws' ('-':w')     = delete w'  ws'
    insertKeyw ws' ('+':w')     = union [w'] ws'
    insertKeyw ws' ""           =            ws'
    insertKeyw ws'      w'      = union [w'] ws'

mergeGeoTags            :: String -> Attrs -> Attrs
mergeGeoTags url a      = merge (matchSubex "https?://maps.google.*[?&]ll=({ll}[-,.0-9]+)&(.*&)?z=({z}[0-9]+)([^0-9].*)?" url)
    where
    merge p@[("ll",pos),("z",_zoom)]    = M.insert keyGoogleMaps ( "http://maps.google.com/maps?"
                                                                   ++
                                                                   intercalate "&" (map (\ (x,y) -> x ++ "=" ++ y) p)
                                                                   ++
                                                                   "&t=k"                               -- map type is satelite
                                                                 )
                                          .
                                          M.insert keyGeoCode    pos
                                          $ a
    merge _                             = a

-- ------------------------------------------------------------

mvPic   :: Name -> Config -> Path -> Pic -> IOE Pic
mvPic newName c p pic
    = do
      _ <- mapM renameCopy $ (map show . M.keys . load theCopies $ pic)
      return $ store theId newName pic
    where
    imgtype     = getImgType c

    renameCopy  :: Name -> IOE ()
    renameCopy  dir
        = do
          when (isAl pic)               -- rename album dir
               (mvFile src dir)
          mvFile (src `addExtension` imgtype) (dst `addExtension` imgtype)
        where
        src = dir </> joinPath p
        dst = dir </> joinPath (init p) </> newName

-- ------------------------------------------------------------

importExifAttrs :: Config -> Path -> Pic -> IOE Pic
importExifAttrs c _p pic
    = do
      orig <- findOrig base orig'
      when (null orig)
           ( throwError $ "importExifAttrs: original image " ++ show orig'' ++ " not found" )
      up <- upToDate orig''
      if up
         then return pic
         else do
              raw <- findRaw base orig
              xmp <- findXmp base orig
              newData <- allImgAttrs [] c base orig raw xmp
              let pic' = change theAttrs (mergeAttrs newData)
                         >>>
                         store theRaw raw
                         >>>
                         store theXmp xmp
                         $ pic
              rnf pic' `seq` return pic'
    where
    orig'       = picOrig pic
    orig''      = base </-> orig'

    modified    = fromMaybe "" . M.lookup fileModificationKey . picAttrs $ pic

    base        = getImportBase c

    force       = optON  optForceExif c
    dry         = optOFF optForceExif c

    upToDate f
        | dry           = return True
        | force         = return False
        | otherwise     = liftIO $ fileNewerThanDate modified f

-- ------------------------------------------------------------

allImgAttrs     :: [String] -> Config -> String -> String -> String -> String -> IOE Attrs
allImgAttrs opts c base orig raw xmp
    = do
      exifDataOrig <- imgAttrs    (base </-> orig)
      exifDataRaw  <- imgAttrs    (base </-> raw)
      xmpData      <- imgAttrsXmp (base </-> xmp)
      return ( ( ( exifDataRaw
                   `M.union` exifDataOrig
                 )
                 `M.union` xmpData
               )
               `M.union` fileData
             )
    where
    debug       = optON  optDebug     c
    exifAttrs t = parseExif (confPicAttrs c) t

    imgAttrs ""
        = return $ emptyAttrs
    imgAttrs f
        = do
          t <- execFct debug (["exiftool"] ++ opts ++ [f]) -- don't use "-s" option
          let res = exifAttrs t
          return $ res

    imgAttrsXmp ""
        = return $ emptyAttrs
    imgAttrsXmp _f
        = do
          return $ emptyAttrs   -- parseXmp t

    fileData
        = exifAttrs . unlines $
          [ "RefOrig : " ++ orig
          , "RefRaw : "  ++ raw
          , "RefXmp : "  ++ xmp
          ]


-- ------------------------------------------------------------

importOrig      :: Config -> Path -> Pic -> IOE Pic
importOrig c p pic
    = do
      ex <- existsSrc
      when (not ex)
           ( throwError $ "importOrig: original file " ++ show src ++ " does not exist" )
      up <- upToDate
      if (not up || not existsCpy)
         then do
              mkDirectoryPath dst
              copy
              geo@(Geo x y) <- getImageSize dst
              if (x == 0 && y == 0)
                 then return pic
                 else do
                      let pic' = change theCopies (M.insert cpyKey (Copy geo)) pic
                      return (rnf pic' `seq` pic')
         else return pic
    where
    cpyKey      = newAtom dir
    existsCpy   = M.member cpyKey . load theCopies $ pic
    existsSrc   = liftIO $ doesFileExist src

    copy
        | extension src == extension dst        -- simple copy
            = liftIO $ copyFile src dst
        | otherwise                             -- conversion with convert command
            = do
              _ <- execFct debug shellcmd
              return ()

    upToDate
        | dry           = return True
        | force         = return False
        | otherwise     = liftIO $ fileNewerThanFile src dst

    src         = base </-> picOrig pic
    dst         = dir  </> joinPath p `addExtension` imgtype

    base        = getImportBase c
    dir         = getOrgDir     c
    imgtype     = getImgType    c

    debug       = optON  optDebug     c
    force       = optON  optForceOrig c
    dry         = optOFF optForceOrig c

    shellcmd
        -- | extension src == extension dst
        --    = [ "cp", src, dst, "&&", "chmod", "644", dst ]
        | otherwise
            = [ "convert", "-quality", "90", src, dst ]

-- ------------------------------------------------------------

createCopy      :: Config -> Path -> Size -> Pic -> IOE Pic
createCopy c p s pic
    = do
      ex <- existsSrc
      when (not ex && not dry)
           ( throwError $ "createCopy: original file " ++ show src ++ " does not exist" )
      mkDirectoryPath dst
      up <- upToDate
      if (not up || not existsCpy)
         then do
              resize
              geo@(Geo x y) <- getImageSize dst
              if (x == 0 && y == 0)
                 then return pic
                 else do
                      let pic' = change theCopies (M.insert cpyKey (Copy geo)) pic
                      return (rnf pic' `seq` pic')
         else return pic
    where
    cpyKey      = newAtom . sizeDir $ s
    existsCpy   = M.member cpyKey . load theCopies $ pic
    existsSrc   = liftIO $ doesFileExist src

    resize
        = resizeImage debug src dst (crGeo aspect) (rGeo aspect)
          where
          rGeo Fix = cGeo
          rGeo _   = resizeGeo sGeo cGeo

          crGeo Fix     = cropGeo sGeo cGeo
          crGeo Pad     = (sGeo, Geo (-1) (-1))
          crGeo Crop    = (sGeo, emptyGeo)

          sGeo   = maybe emptyGeo copyGeo . M.lookup (newAtom dir) . picCopies $ pic
          cGeo   = sizeGeo    s
          aspect = sizeAspect s

    upToDate
        | dry           = return True
        | force         = return False
        | otherwise     = liftIO $ fileNewerThanFile src dst

    dir         = getOrgDir  c
    imgtype     = getImgType c

    debug       = optON  optDebug     c
    force       = optON  optForceCopy c
    dry         = optOFF optForceCopy c

    img         = joinPath p `addExtension` imgtype
    src         = dir       </> img
    dst         = sizeDir s </> img

-- ------------------------------------------------------------
--
-- | image resize
--
-- croping may be given by @(cropWidth, cropHeight, xOffset, yOffset)@ for
-- generating an image with a specific aspect ration

resizeImage     :: Bool -> String -> String -> (Geo, Geo) -> Geo -> IOE ()
resizeImage debug src dst (Geo cw ch, Geo xoff yoff) (Geo w h)
    = do
      _ <- execFct debug shellCmd
      return ()
    where
    unsharp     = [] -- ["-unsharp", "0.7x0.7+1.0+0.05"] -- sharpen option removed
    resize      = ["-thumbnail", show w ++ "x" ++ show h ++ "!"]
    resize1     = ["-geometry", show w ++ "x" ++ show h, "-thumbnail", show w ++ "x" ++ show h]
    quality     = ["-quality", "85"]
    interlace   = [ "-interlace", "Plane" ]
    isPad       = (xoff == (-1) && yoff == (-1))
    isCrop      = (xoff > 0     || yoff > 0)
    cmdName
        | isPad         = [ "montage" ]
        | otherwise     = [ "convert" ]
    cmdArgs
        | isPad         = resize1
                          ++ [ "-background", "#333333" ]
                          -- ++ [ "-size", show (2*w) ++ "x" ++ show (2*h) ] -- this gives too low quality
                          ++ [ src, dst ]
        | isCrop        = [ "-crop", show cw ++ "x" ++ show ch ++ "+" ++ show xoff ++ "+" ++ show yoff
                          , src, "miff:-"
                          , "|"
                          , "convert"
                          ]
                          ++ resize ++ unsharp ++ quality
                          ++ ["miff:-", dst ]
        | otherwise     = resize ++ unsharp
                          ++ [ src, dst ]
    shellCmd    = cmdName
                  ++ interlace
                  ++ quality
                  ++ cmdArgs

resizeGeo       :: Geo -> Geo -> Geo
resizeGeo sGeo@(Geo sw sh) (Geo dw dh)
    | sw <= dw && sh <= dh              -- source fits into display
        = sGeo                          -- no downsizing, no magnification

    | sw * dh >= dw * sh                -- source wider than display
        = Geo dw (dw * sh `div` sw)     -- maximum width, height scaled down

    | otherwise                         -- source higher than display
        = Geo (dh * sw `div` sh) dh     -- maximum height, width scaled down


cropGeo         :: Geo -> Geo -> (Geo, Geo)
cropGeo (Geo sw sh) (Geo dw dh)
    | sw *dh >= dw * sh                 -- source wider than reqired
        = (Geo sw' sh, Geo xoff 0)
    | otherwise                         -- sorce highter than required
        = (Geo sw sh', Geo 0 yoff)
    where
    sw'  = dw * sh `div` dh
    xoff = (sw - sw') `div` 2           -- cut off left and right parts
    sh'  = dh * sw `div` dw
    yoff = (sh - sh') `div` 3           -- cut off 1/3 from top and 2/3 from bottom
                                        -- else important parts like heads are cut off (Ouch!!)

-- ------------------------------------------------------------

getImageSize    :: String -> IOE Geo
getImageSize f
    = do
      res <- execFct False ["identify", "-ping", f]
             `catchError`
             const (return "")
      return $ parseGeoFromIdentify res

{- old
      return ( maybe (Geo 0 0) (readGeo . head) (matchRegex geometryRE res) )
    where
    geometryRE  :: Regex
    geometryRE
        = mkRegex ( "(" ++ digit1 ++ digit0 ++ "*x" ++ digit1 ++ digit0 ++ "*)" )
          where
          digit0 = "[0-9]"
          digit1 = "[1-9]"
-}

parseGeoFromIdentify    :: String -> Geo
parseGeoFromIdentify s  = build (matchSubex "\\A[ ]({w}[1-9][0-9]*)x({h}[1-9][0-9]*)[ ]\\A" s)
    where
    build [("w",w),("h",h)]     = Geo (read w) (read h)
    build _                     = Geo 0 0

-- ------------------------------------------------------------
--
-- call of external programs
-- and file handling

-- | quote a string to be used as command line argument for a system call

addArg          :: String -> String
addArg ";"      = " ;"
addArg "|"      = " |"
addArg "||"     = " ||"
addArg "&&"     = " &&"
addArg ">"      = " >"
addArg "2>"     = " 2>"
addArg t        = " '" ++ concatMap (\ c -> if c == '\'' then "\\'" else [c]) t ++ "\'"

-- | execute a shell command and return exit code

exec            :: String -> IOE ()
exec cmd
    = do
      rc <- liftIO $ system cmd
      exit rc
    where
    exit ExitSuccess       = return ()
    exit (ExitFailure rc ) = throwError ("command " ++ show cmd ++ " exited with rc=" ++ show rc)

-- ------------------------------------------------------------

execFct         :: Bool -> [String] -> IOE String

execFct _ []
    = return ""

execFct debug (cmd : args)
    = do
      pid <- liftIO $ getProcessID
      let tmpName = "/tmp/album-" ++ show pid
      let errDev  = "/dev/null"
      let ioReDir = [">", tmpName, "2>", errDev]
      let command = cmd ++ concatMap addArg args
      when debug
           ( liftIO $ hPutStrLn stderr ("executed: " ++ command) )
      let command' = command ++ concatMap addArg ioReDir
      exec command'
      catchError ( do
                   res <- liftIO $ B.readFile tmpName
                   rmFile tmpName
                   return (C.unpack res)
                 ) ( \ _ -> return "" )

-- ------------------------------------------------------------

mkDirectoryPath         :: String -> IOE ()
mkDirectoryPath f
    = do
      ex <- liftIO $ doesDirectoryExist dir
      when (not ex)
           ( ( liftIO $ createDirectoryIfMissing True dir )
             `mapError`
             (("createDirectory " ++ dir ++ " failed: ") ++)
           )
    where
    dir = dirPath f

-- ------------------------------------------------------------

mvFile          :: String -> String -> IOE ()
mvFile src dst
    = ( do
        ex <- liftIO $ doesFileExist src
        when ex (liftIO $ renameFile src dst)
      )
      `mapError` (("rename " ++ show src ++ " " ++ show dst ++ " failed: ") ++)

-- ------------------------------------------------------------

rmFile          :: String -> IOE ()
rmFile f
    = ( do
        ex <- liftIO $ doesFileExist f
        when ex (liftIO $ removeFile f)
      )
      `mapError` (("remove file " ++ show f ++ " failed: ") ++)

-- ------------------------------------------------------------

lnFile          :: String -> String -> IOE ()
lnFile src dst
    = ( do
        ex <- liftIO $ doesFileExist src
        when ex (liftIO $ createLink src dst)
      )
      `mapError` ((unwords ["link file", show src, show dst, "failed: "]) ++)

-- ------------------------------------------------------------

cpFile          :: String -> String -> IOE ()
cpFile src dst
    = ( do
        ex <- liftIO $ doesFileExist src
        when ex (liftIO $ copyFile src dst)
      )
      `mapError` ((unwords ["copy file", show src, show dst, "failed: "]) ++)

-- ------------------------------------------------------------

rmDir           :: String -> IOE ()
rmDir d
    = ( do
        ex <- liftIO $ doesDirectoryExist d
        when ex (liftIO $ removeDirectoryRecursive d)
      )
      `mapError` (("remove dir " ++ show d ++ " failed: ") ++)

-- ------------------------------------------------------------

mkBackupFile    :: String -> String -> IOE ()
mkBackupFile bak f
    = do
      ex <- liftIO $ doesFileExist f
      when ex
           ( liftIO $ copyFile f (f ++ bak) )

-- ------------------------------------------------------------
--
-- simple IO actions for time stamps and date and time functions
-- time is always tranformed into local time

formatDateTime  :: IO UTCTime -> IO String
formatDateTime timeStamp
    = catch'
      ( do
        ctime <- timeStamp
        tzone <- getCurrentTimeZone
        let ltime = utcToLocalTime tzone ctime
        return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ltime
      )
      ( \_ -> return "" )

getTimeStamp    :: IO String
getTimeStamp    = formatDateTime getCurrentTime

-- | read the last modified time stamp of a file

fileLastModified        :: String -> IO String
fileLastModified f      = formatDateTime (getModificationTime f)

-- | compare 2 file stamps
--
-- @fileNewerThanFile reference file@
-- if reference does not exist, return False,
-- else if file does not exist, return False
-- else compare time stamps

fileNewerThanFile       :: String -> String -> IO Bool
fileNewerThanFile ref f
    = do
      mf   <- fileLastModified f
      mref <- fileLastModified ref
      let n = ( not (null mf)
               &&
               not (null mref)
               &&
               mref <= mf
             )
      -- putStrLn ("file newer file=" ++ show f ++ "," ++ show mf ++ " ref=" ++ show ref ++ "," ++ show mref ++ " status=" ++ show n)
      return n

fileNewerThanFiles      :: [String] -> String -> IO Bool
fileNewerThanFiles [] _f
    = return True
fileNewerThanFiles (r:refs) f
    = do
      newer <- fileNewerThanFile r f
      if newer
         then fileNewerThanFiles refs f
         else return False

-- | compare a file stamp with a time stamp
--
-- @fileNewerThanDate dateRef file@
-- if dateRef is empty, return False,
-- else if file does not exist, return True
-- else compare time stamps

fileNewerThanDate       :: String -> String -> IO Bool
fileNewerThanDate dref f
    = if null dref
      then return False
      else do
           mf   <- fileLastModified f
           return ( not (null dref)
                    &&
                    dref <= mf
                  )

fileNewerThanDates      :: [String] -> String -> IO Bool
fileNewerThanDates [] _f
    = return True
fileNewerThanDates (r:refs) f
    = do
      newer <- fileNewerThanDate r f
      if newer
         then fileNewerThanDates refs f
         else return False

-- ------------------------------------------------------------

rmCopies                :: FilePath -> String -> [String] -> IOE ()
rmCopies p ext dirs
    = do
      liftIO $ putStrLn ("rmCopies " ++ show (p,ext,dirs))
      mapM_ rmFile . map ((`addExtension` ext) . (</> p)) $ dirs

-- ------------------------------------------------------------

cleanupDir              :: Bool -> FilePath -> String -> [String] -> IOE ()
cleanupDir execute dir ext fl
    = do
      ex <- liftIO $ doesDirectoryExist dir             -- dir must exist
      when ex $
           do
           fl' <- liftIO $ getDirectoryContents dir
           mapM_ cleanEntry fl'
    where
    fle = map (`addExtension` ext) fl
    cleanEntry e
        | e `elem` fle
          ||
          e `elem` [".",".."]
            = return ()
        | otherwise
            = do
              isd <- liftIO $ doesDirectoryExist e'
              if isd
                 then when (not (e `elem` fl)) $
                           do
                           when execute (rmDir e')
                           puts msg2
                 else do
                      when execute (rmFile e')
                      puts msg
        where
        e'                      = dir </> e
        puts                    = liftIO . hPutStrLn stderr
        msg     | execute       = "unused file removed " ++ show e'
                | otherwise     = "unused file found "   ++ show e'
        msg2    | execute       = "unused dir  removed " ++ show e'
                | otherwise     = "unused dir  found "   ++ show e'

-- ------------------------------------------------------------

cpCopies        :: String -> Path -> Path -> [String] -> IOE ()
cpCopies ext dst src copies
    = do
      mapM_ link copies
    where
    link dir
        = do
          liftIO $ putStrLn $ unwords ["cp", srcFile, dstFile]
          mkDirectoryPath dstFile
          cpFile srcFile  dstFile       -- lnFile replaced by cpFile, vmware volume don't support links
        where
        srcFile = dir </> listToPath src `addExtension` ext
        dstFile = dir </> listToPath dst `addExtension` ext

-- ------------------------------------------------------------

findRelFile :: (String -> String) -> (String -> String -> String) -> String -> String -> IOE String
findRelFile cBaseName cRelName base f
    = liftIO $
      do
      ex <- doesFileExist fname'
      return ( if ex then fname else "")
    where
    fname0 = cBaseName f
    fname  = cRelName  fname0 f
    fname' = base </-> fname


findOrig        :: String -> String -> IOE String
findOrig        = findRelFile id const

findRaw'        :: String -> String -> String -> IOE String
findRaw' ext    = findRelFile
                  ( (`addExtension` ext) . removeRawVersion . removeExtension . baseName )
                  ( \ fn -> (</> fn) . dirPath . dirPath )

findRaws'       :: [String] -> String -> String -> IOE String
findRaws' exts  base f
                = do
                  fns <- sequence . map (\ e -> findRaw' e base f) $ exts
                  return . head . (++ [""]) . filter (not . null) $ fns

findRaw         :: String -> String -> IOE String
findRaw         = findRaws' ["nef", "rw2", "jpg"]

findXmp         :: String -> String -> IOE String
findXmp         = findRelFile
                  ( (`addExtension` "xmp") . removeExtension . baseName )
                  ( \ fn -> (</> fn) . dirPath . dirPath )

removeRawVersion :: String -> String
removeRawVersion                                    -- remove raw conversion version
                = sed remSubNo "([_a-zA-Z]+[0-9]+)-[0-9]+"
                  where
                  remSubNo = takeWhile (/= '-')

-- ------------------------------------------------------------
