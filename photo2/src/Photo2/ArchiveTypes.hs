module Photo2.ArchiveTypes
where

import           Control.DeepSeq

import           Data.Atom
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M

import           Text.XML.HXT.Core hiding ( mkLeaf )

import           Data.Tree.Class
import           Data.Tree.NTree.TypeDefs

import           Photo2.FilePath

import           System.IO

-- ------------------------------------------------------------
--
-- the global state

data AppState           = AppState { albums      :: AlbumTree
                                   , archiveName :: Href
                                   , config      :: Config
                                   , configName  :: Href
                                   , status      :: Status
                                   , cwd         :: Path
                                   , writeLog    :: String -> IO ()
                                   , writeRes    :: String -> IO ()
                                   , templates   :: HtmlTemplates
                                   }

type Options            = AssocList Name Value

data Status             = Running Int
                        | Exc String
                          deriving (Eq, Show)

type HtmlTemplates      = Map String XmlTrees

-- ------------------------------------------------------------

type AlbumEntry         = (Path, Pic)
type Name               = String
type Value              = String
type Path               = [Name]

-- ------------------------------------------------------------
-- config data

data Config             = Config { confAttrs    :: Attrs
                                 , confPicAttrs :: PicAttrs
                                 , confLayouts  :: Layouts
                                 , confDict     :: Dictionaries
                                 , confSizes    :: Sizes
                                 }
                          deriving (Show)

type Attrs              = Map Atom Value
type PicAttrs           = [(Value, Name)]

type Layouts            = Map Atom Layout
data Layout             = Layout { layoutType  :: Value
                                 , layoutAttrs :: Attrs
                                 , layoutPages :: Pages
                                 }
                          deriving (Show)

type Dictionaries       = Map Atom Dictionary
type Dictionary         = Attrs

type Pages              = Map Atom Page
type Page               = Attrs

type Sizes              = [Size]
data Size               = Size { sizeDir    :: String
                               , sizeGeo    :: Geo
                               , sizeAspect :: AspectRatio
                               }
                          deriving (Show, Eq)

data AspectRatio        = Fix | Pad | Crop
                          deriving (Show, Enum, Eq, Ord)

data Geo                = Geo { geoWidth  :: Int
                              , geoHeight :: Int
                              }
                          deriving (Show, Eq)

-- ------------------------------------------------------------
-- album data types

data Archive            = Archive { archConfRef   :: Href
                                  , archRootAlbum :: AlbumTree
                                  }
                          deriving (Show)

type AlbumTree          = NTree Pic
data Pic                = Pic { picId     :: Name
                              , isAl      :: Bool
                              , picRef    :: Href
                              , picOrig   :: Href
                              , picRaw    :: Href
                              , picXmp    :: Href
                              , picCopies :: Copies
                              , picAttrs  :: Attrs
                              , picErrs   :: Errs
                              , picEdited :: Bool
                              }
                          deriving (Show, Eq)

type Copies             = Map Atom Copy
data Copy               = Copy { copyGeo  :: Geo
                               }
                          deriving (Show, Eq)

type Href               = String
type Errs               = [String]

-- ------------------------------------------------------------

type Getter s a         = s -> a
type Setter s a         = a -> s -> s
type Selector s a       = (Getter s a, Setter s a)

sub                     :: Selector b c -> Selector a b -> Selector a c
sub (g2, s2) (g1, s1)   = ( g2 . g1
                          , s1s2
                          )
                          where
                          s1s2 x s = s'
                              where
                              x1  = g1 s
                              x1' = s2 x x1
                              s'  = s1 x1' s

change                  :: Selector s a -> (a -> a) -> (s -> s)
change (g, s) f x       = s (f (g x)) x

load                    :: Selector s a -> s -> a
load (g, _s) x          = g x

store                   :: Selector s a -> a -> (s -> s)
store s v               = change s (const v)

-- ------------------------------------------------------------

type AppSelector a      = Selector AppState a

selAlbums               :: AppSelector AlbumTree
selAlbums               = (albums,      \ x s -> s {albums = x})

selArchiveName          :: AppSelector Href
selArchiveName          = (archiveName, \ x s -> s {archiveName = x})

selConfig               :: AppSelector Config
selConfig               = (config,      \ x s -> s {config = x})

selConfigName           :: AppSelector Href
selConfigName           = (configName,  \ x s -> s {configName = x})

selStatus               :: AppSelector Status
selStatus               = (status,      \ x s -> s {status = x})

selWd                   :: AppSelector Path
selWd                   = (cwd,         \ x s -> s {cwd = x})

selWriteLog             :: AppSelector (String -> IO ())
selWriteLog             = (writeLog,    \ x s -> s {writeLog = x})

selWriteRes             :: AppSelector (String -> IO ())
selWriteRes             = (writeRes,    \ x s -> s {writeRes = x})

selConfigAttrs          :: AppSelector Attrs
selConfigAttrs          = (confAttrs,   \ x c -> c {confAttrs = x}) `sub` selConfig

selConfigPicAttrs       :: AppSelector PicAttrs
selConfigPicAttrs       = (confPicAttrs,   \ x c -> c {confPicAttrs = x}) `sub` selConfig

selConfigAttr           :: Name -> AppSelector Value
selConfigAttr k         = (fromMaybe "" . M.lookup (newAtom k),
                                \ v as -> M.insert (newAtom k) v as)  `sub` selConfigAttrs

selTemplates            :: AppSelector HtmlTemplates
selTemplates            = (templates, \ x s -> s {templates = x})

selTemplate             :: String -> AppSelector XmlTrees
selTemplate k           = (fromMaybe [] . M.lookup k,
                                \ v tm -> M.insert k v tm)  `sub` selTemplates

-- ------------------------------------------------------------

type PicSelector a      = Selector Pic a

theId                   :: PicSelector Name
theId                   = ( picId,     \ n x -> x { picId     = n } )

theCopies               :: PicSelector Copies
theCopies               = ( picCopies, \ c x -> x { picCopies = c } )

theOrig                 :: PicSelector Href
theOrig                 = ( picOrig,   \ r x -> x { picOrig   = r } )

theRaw                  :: PicSelector Href
theRaw                  = ( picRaw,   \ r x -> x { picRaw     = r } )

theXmp                  :: PicSelector Href
theXmp                  = ( picXmp,   \ r x -> x { picXmp     = r } )

theEdited               :: PicSelector Bool
theEdited               = ( picEdited, \ e x -> x { picEdited = e } )

theAttrs                :: PicSelector Attrs
theAttrs                = ( picAttrs,  \ a x -> x { picAttrs  = a } )

theRef                  :: PicSelector Href
theRef                  = ( picRef,    \ r x -> x { picRef    = r } )

-- ------------------------------------------------------------

emptyAppState           :: AppState
emptyAppState           = AppState { albums       = emptyAlbumTree
                                   , archiveName  = ""
                                   , config       = emptyConfig
                                   , configName   = ""
                                   , status       = Running 0
                                   , cwd          = []
                                   , writeLog     = hPutStrLn stderr
                                   , writeRes     = hPutStrLn stdout
                                   , templates    = emptyTemplates
                                   }

emptyConfig             :: Config
emptyConfig             = Config { confAttrs    = emptyAttrs
                                 , confPicAttrs = []
                                 , confLayouts  = M.empty
                                 , confDict     = M.empty
                                 , confSizes    = []
                                 }

emptyPic                :: Pic
emptyPic                = Pic { picId     = emptyName
                              , isAl      = False
                              , picRef    = ""
                              , picOrig   = ""
                              , picRaw    = ""
                              , picXmp    = ""
                              , picCopies = M.empty
                              , picAttrs  = M.empty
                              , picErrs   = []
                              , picEdited = False
                              }

clearPic                :: Pic -> Pic
clearPic p              = p { picOrig   = ""
                            , picRaw    = ""
                            , picXmp    = ""
                            , picCopies = M.empty
                            , picAttrs  = M.empty
                            , picErrs   = []
                            , picEdited = False
                            }

emptyName               :: Name
emptyName               = ""

emptyPath               :: Path
emptyPath               = []

albumTree               :: Pic -> AlbumTree
albumTree               = mkLeaf

emptyAlbumTree          :: AlbumTree
emptyAlbumTree          = albumTree emptyPic

emptyGeo                :: Geo
emptyGeo                = Geo 0 0

emptyAttrs              :: Attrs
emptyAttrs              = M.empty

emptyTemplates          :: HtmlTemplates
emptyTemplates          = M.empty

-- ------------------------------------------------------------

formatAlbumTree         :: AlbumTree -> String
formatAlbumTree         = formatTree showPic

-- ------------------------------------------------------------

xpAlbumTree             :: PU AlbumTree
xpAlbumTree             = xpAlt ( \ (NTree e _) -> fromEnum (isAl e) )
                          [ xpElem "picture" $ xpPicture
                          , xpElem "album"   $ xpAlbum
                          ]

xpPicture               :: PU AlbumTree
xpPicture               = xpWrap ( \ p -> NTree p []
                                 , \ (NTree p _) -> p
                                 ) $
                          xpWrapPic False

xpAlbum                 :: PU AlbumTree
xpAlbum                 = xpWrap ( uncurry NTree
                                 , \ (NTree p cs) -> (p, cs)
                                 ) $
                          xpPair ( xpWrapPic True )
                                 ( xpList $ xpAlbumTree )

xpWrapPic               :: Bool -> PU Pic
xpWrapPic isa           = xpWrap ( \ (es,i,h,(o,r,x),cs,as)
                                   -> Pic { picId     = i
                                          , isAl      = isa
                                          , picRef    = h
                                          , picOrig   = o
                                          , picRaw    = r
                                          , picXmp    = x
                                          , picCopies = cs
                                          , picAttrs  = as
                                          , picErrs   = es
                                          , picEdited = False
                                          }
                                 , \ p -> ( picErrs p
                                          , picId   p
                                          , picRef  p
                                          , (picOrig p, picRaw p, picXmp p)
                                          , picCopies p
                                          , picAttrs  p
                                          )
                                 ) $
                          xp6Tuple ( xpErrs )
                                   (                xpAttr "id"   $ xpName )
                                   ( xpDefault "" $ xpAttr "href" $ xpText )
                                   ( xpDefault ("", "", "") $
                                     xpElem "orig" $
                                     xpTriple (xpDefault "" $ xpAttr "href1" $ xpText )
                                     (xpDefault "" $ xpAttr "raw1"  $ xpText )
                                     (xpDefault "" $ xpAttr "xmp"   $ xpText )
                                   )
                                   ( xpMap "copy" "base" xpAtom xpCopy )
                                   ( xpAttrs )

xpCopy                  :: PU Copy
xpCopy                  = xpWrap ( Copy
                                 , copyGeo
                                 ) $
                          xpAttr "geometry" $ xpGeo

xpErrs                  :: PU Errs
xpErrs                  = xpList $
                          xpElem "error" $
                          xpHtmlText

xpName                  :: PU Name
xpName                  = xpText

xpAtom                  :: PU Atom
xpAtom                  = xpWrap (newAtom, show) $
                          xpName

xpAlbumEntry            :: PU AlbumEntry
xpAlbumEntry            = xpElem "entry" $
                          xpPair ( xpAttr "path" $
                                   xpWrap ( splitPath . mkRelPath, mkAbsPath . joinPath ) $
                                   xpText
                                 )
                                 ( xpWrap ( \ (NTree e _) -> e
                                          , albumTree
                                          ) $
                                   xpAlbumTree
                                 )

-- ------------------------------------------------------------

instance XmlPickler Archive     where xpickle = xpArchive
instance XmlPickler Config      where xpickle = xpConfig
instance XmlPickler Size        where xpickle = xpSize
instance XmlPickler Geo         where xpickle = xpGeo
instance XmlPickler AspectRatio where xpickle = xpAspectRatio

xpArchive               :: PU Archive
xpArchive               = xpElem "archive" $
                          xpAddFixedAttr "xmlns" "http://muehle.welt.all/photos.dtd" $
                          xpWrap ( uncurry Archive
                                 , \ a -> (archConfRef a, archRootAlbum a)
                                 ) $
                          xpPair ( xpAttr "config" $ xpText )
                                 ( xpAlbumTree )

xpConfig                :: PU Config
xpConfig                = xpElem "config" $
                          xpWrap ( \ (x1, x2, x3, x4, x5) -> Config x1 x2 x3 x4 x5
                                 , \ c -> (confAttrs c, confPicAttrs c, confLayouts c, confDict c, confSizes c)
                                 ) $
                          xp5Tuple xpAttrs xpPicAttrs xpLayouts xpDictionaries xpSizes
               
xpAttrs                 :: PU Attrs
xpAttrs                 = xpMap "attr" "name" xpAtom xpHtmlText

xpPicAttrs              :: PU PicAttrs
xpPicAttrs              = xpDefault [] $
                          xpElem "picture-attributes" $
                          xpList $
                          xpElem "attr" $
                          xpPair ( xpHtmlText             )
                                 ( xpAttr "name" $ xpName )

xpHtmlText              :: PU String
xpHtmlText              = xpWrap ( showXML, readHTML ) $ xpTrees
                          where
                          showXML  = concat . runLA ( xshowEscapeXml unlistA )
                          readHTML = runLA hread                                -- hread ignores not wellformed attributes, e.g. unescaped &s in URLs

xpLayouts               :: PU Layouts
xpLayouts               = xpMap "layout" "id" xpAtom xpLayout

xpLayout                :: PU Layout
xpLayout                = xpWrap ( uncurry3 Layout
                                 , \ l -> (layoutType l, layoutAttrs l, layoutPages l)
                                 ) $
                          xpTriple ( xpAttr "type" $ xpText )
                                   ( xpAttrs )
                                   ( xpPages )

xpDictionaries          :: PU Dictionaries
xpDictionaries          = xpMap "dictionary" "id" xpAtom xpDictionary

xpDictionary            :: PU Dictionary
xpDictionary            = xpAttrs

xpPages                 :: PU Pages
xpPages                 = xpMap "page" "type" xpAtom xpAttrs

xpSizes                 :: PU Sizes
xpSizes                 = xpList xpSize

xpSize                  :: PU Size
xpSize                  = xpElem "size" $
                          xpWrap ( uncurry3 Size
                                 , \ s -> (sizeDir s, sizeGeo s, sizeAspect s)
                                 ) $
                          xpTriple ( xpElemWithAttrValue "attr" "name" "dir"      $ xpText  )
                                   ( xpElemWithAttrValue "attr" "name" "geometry" $ xpickle )
                                   ( xpWrap ( fromMaybe Pad
                                            , Just
                                            ) $
                                     xpOption $
                                     xpElemWithAttrValue "attr" "name" "aspect-ratio" $
                                     xpickle
                                   )

xpAspectRatio           :: PU AspectRatio
xpAspectRatio           = xpWrap ( readAspect
                                 , showAspect
                                 ) $ xpText

xpGeo                   :: PU Geo
xpGeo                   = xpWrap ( readGeo
                                 , showGeo
                                 ) $ xpText

-- ----------------------------------------

readAspect              :: String -> AspectRatio
readAspect "fix"        = Fix
readAspect "pad"        = Pad
readAspect "crop"       = Crop
readAspect _            = Crop

showAspect              :: AspectRatio -> String
showAspect Fix          = "fix"
showAspect Pad          = "pad"
showAspect Crop         = "crop"

-- ----------------------------------------

nats    :: [Int]
nats    = [1..]

-- ----------------------------------------

readGeo                 :: String -> Geo
readGeo s               = Geo (read w) (read h)
                          where
                          s'      = filter (\ x -> isDigit x || x == 'x') s
                          (w, h') = span isDigit s'
                          h       = drop 1 h'

showGeo                 :: Geo -> String
showGeo g               = show (geoWidth g) ++ "x" ++ show (geoHeight g)

-- ------------------------------------------------------------

showPic                 :: Pic -> String
showPic p               = concat . intersperse "\n" . filter (not . null) $
                          [ showC "id"   picId
                          , showC "ref"  picRef
                          , showC "orig" picOrig
                          , showC "raw"  picRaw
                          , showC "xmp"  picXmp
                          ]
                          where
                          showC k f
                              | null v = ""
                              | otherwise = k ++ ":\t" ++ v
                              where
                              v = f p

-- ------------------------------------------------------------

instance NFData AppState where
    rnf (AppState a n cf cfn s wd _wl _wr tm)
                        = rnf a `seq` rnf n `seq` rnf cf `seq` rnf cfn `seq` rnf s `seq` rnf wd `seq` rnf tm

instance NFData Status where
    rnf (Running i)     = rnf i
    rnf (Exc msg)       = rnf msg

instance NFData Config where
    rnf (Config ca cp cl cd cs)
                        = rnf ca `seq` rnf cp `seq` rnf cl `seq` rnf cd `seq` rnf cs

instance NFData Layout where
    rnf (Layout lt la lp)
                        = rnf lt `seq` rnf la `seq` rnf lp

instance NFData Size where
    rnf (Size sd sg sa) = rnf sd `seq` rnf sg `seq` rnf sa

instance NFData AspectRatio where

instance NFData Geo where
    rnf (Geo w h)       = rnf w `seq` rnf h

instance NFData Archive where
    rnf (Archive ac ar) = rnf ac `seq` rnf ar

instance NFData Pic where
    rnf (Pic pj ia pr po pw px pc pa pe pd)
                        = rnf pj `seq` rnf ia `seq` rnf pr `seq` rnf po `seq`
                          rnf pw `seq` rnf px `seq` rnf pc `seq` rnf pa `seq` rnf pe `seq` rnf pd

instance NFData Copy where
    rnf (Copy cg)       = rnf cg

-- ------------------------------------------------------------
