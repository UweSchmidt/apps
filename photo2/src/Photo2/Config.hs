module Photo2.Config
where

import           Data.Atom
import           Data.Char
import qualified Data.Map               as M
import           Data.Maybe

import           Photo2.ArchiveTypes
import           Photo2.FilePath

-- ------------------------------------------------------------

statusOk                :: Status -> Bool
statusOk (Exc _)        = False
statusOk _              = True

startTr                 :: Status -> Status
startTr (Running l)     = Running (l + 1)
startTr e               = e

stopTr                  :: Status -> Status
stopTr (Running l)      = Running (l - 1)
stopTr e                = e

-- ------------------------------------------------------------

keyAlbumDir             :: Atom
keyAlbumDir             = newAtom "album-dir"

keyCreateDate           :: Atom
keyCreateDate           = newAtom "exif:CreateDate"

keyKeywords             :: Atom
keyKeywords             = newAtom "descr:Keywords"

keyGoogleMaps           :: Atom
keyGoogleMaps           = newAtom "descr:GoogleMaps"

keyGeoCode              :: Atom
keyGeoCode              = newAtom "descr:GeoCode"

-- ------------------------------------------------------------

albumDir                :: Config -> FilePath
albumDir                = fromMaybe "album2" . M.lookup keyAlbumDir . confAttrs

albumPath               :: Path -> Config -> FilePath
albumPath p c           = albumDir c </> (joinPath p `addExtension` "xml")

getOpt                  :: String -> Config -> String
getOpt                  = getDefOpt ""

getDefOpt               :: String -> String -> Config -> String
getDefOpt d o           = fromMaybe d . M.lookup (newAtom o) . confAttrs

optON                   :: String -> Config -> Bool
optON o                 = (`elem` ["1","yes","true"]) . map toLower . getOpt o

optOFF                  :: String -> Config -> Bool
optOFF o                = (`elem` ["0","no","false"]) . map toLower . getOpt o

optDebug                :: String
optDebug                = "debug"

optDryRun               :: String
optDryRun               = "dry-run"

optForceStore           :: String
optForceStore           = "store-all"

optForceOrig            :: String
optForceOrig            = "copy-org"

optForceCopy            :: String
optForceCopy            = "copy-copy"

optForceExif            :: String
optForceExif            = "copy-exif"

optImportDialog         :: String
optImportDialog         = "import-dialog"

getImportBase           :: Config -> String
getImportBase           = getDefOpt "../Diakaesten" "import-base"

getImgType              :: Config -> String
getImgType              = getDefOpt "jpg"           "imgtype"

getOrgDir               :: Config -> String
getOrgDir               = getDefOpt "org"           "dir"

-- ------------------------------------------------------------
