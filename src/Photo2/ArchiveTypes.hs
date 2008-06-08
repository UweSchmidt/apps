module Photo2.ArchiveTypes
where

import           Data.Char
import		 Data.List
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as M

import           Text.XML.HXT.Arrow hiding ( mkLeaf )
import           Data.Tree.NTree.TypeDefs

import           Photo2.FilePath

-- ------------------------------------------------------------
--
-- the global state

data AppState	= AppState { albums      :: ! AlbumTree
			   , archiveName :: ! Href
			   , config      :: ! Config
			   , configName  :: ! Href
			   , options     :: ! Options
			   , status      :: ! Status
			   , cwd	 :: ! Path
			   }
		  deriving (Show)

type Options	= AssocList Name Value
data Status	= Running Int
		| Exc String
		  deriving (Eq, Show)

initialAppState	:: AppState
initialAppState	= AppState { albums       = emptyAlbumTree
			   , archiveName  = ""
			   , config       = emptyConfig
			   , configName   = ""
			   , options      = []
			   , status       = Running 0
			   , cwd          = []
			   }

-- ------------------------------------------------------------

type AlbumEntry	= (Path, Pic)
type Name	= String
type Value	= String
type Path	= [Name]

-- ------------------------------------------------------------
-- config data

data Config	= Config { confAttrs   :: Attrs
			 , confLayouts :: Layouts
			 , confDict    :: Dictionaries
			 , confSizes   :: Sizes
			 }
		  deriving (Show)

type Attrs	= Map Name Value

type Layouts	= Map Name Layout
data Layout	= Layout { layoutType  :: Value
			 , layoutAttrs :: Attrs
			 , layoutPages :: Pages
			 }
		  deriving (Show)

type Dictionaries
		= Map Name Dictionary
type Dictionary	= Attrs

type Pages	= Map Name Page
type Page	= Attrs

type Sizes	= [Size]
data Size	= Size { sizeDir    :: String
		       , sizeGeo    :: Geo
		       , sizeAspect :: AspectRatio
		       }
		  deriving (Show, Eq)

data AspectRatio
		= Fix | Pad
		  deriving (Show, Enum, Eq, Ord)

data Geo	= Geo { geoWidth  :: Int
		      , geoHeight :: Int
		      }
		  deriving (Show, Eq)

-- ------------------------------------------------------------
-- album data types

data Archive	= Archive { archConfRef   :: Href
			  , archRootAlbum :: AlbumTree
			  }
		  deriving (Show)

type AlbumTree	= NTree Pic
data Pic	= Pic { picId     :: Name
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

type Copies	= Map Name Copy
data Copy	= Copy { copyGeo  :: Geo
		       }
		  deriving (Show, Eq)

type Href	= String
type Errs	= [String]

-- ------------------------------------------------------------

emptyConfig	:: Config
emptyConfig	= Config { confAttrs = M.empty
			 , confLayouts = M.empty
			 , confDict = M.empty
			 , confSizes = []
			 }

emptyPic	:: Pic
emptyPic	= Pic { picId = emptyName
		      , picRef = ""
		      , picOrig = ""
		      , picRaw = ""
		      , picXmp = ""
		      , picCopies = M.empty
		      , picAttrs = M.empty
		      , picErrs = []
		      , picEdited = False
		      }

emptyName	:: Name
emptyName	= ""

emptyAlbumTree	:: AlbumTree
emptyAlbumTree	= mkLeaf emptyPic

-- ------------------------------------------------------------

formatAlbumTree	:: AlbumTree -> String
formatAlbumTree	= formatTree showPic

-- ------------------------------------------------------------

xpAlbumTree	:: PU AlbumTree
xpAlbumTree
    = xpAlt ( \ (NTree e cs) -> fromEnum ( ( not . null $ cs )
					   ||
					   ( not . null . picRef $ e )
					 )
	    )
      [ xpElem "picture" $ xpPicture
      , xpElem "album"   $ xpAlbum
      ]

xpPicture	:: PU AlbumTree
xpPicture
    = xpWrap ( \ p -> NTree p []
	     , \ (NTree p _) -> p
	     ) $
      xpWrapPic

xpAlbum	:: PU AlbumTree
xpAlbum
    = xpWrap ( uncurry NTree
	     , \ (NTree p cs) -> (p, cs)
	     ) $
      xpPair ( xpWrapPic )
             ( xpList $ xpAlbumTree )


xpWrapPic	:: PU Pic
xpWrapPic
    = xpWrap ( \ (es,i,h,(o,r,x),cs,as)
	       -> Pic { picId     = i
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
	       ( xpMap "copy" "base" xpName xpCopy )
	       ( xpAttrs )


xpCopy	:: PU Copy
xpCopy
    = xpWrap ( Copy
	     , copyGeo
	     ) $
      xpAttr "geometry" $ xpGeo


xpErrs	:: PU Errs
xpErrs	= xpList $
	  xpElem "error" $
	  xpHtmlText

xpName	:: PU Name
xpName	= xpText

xpAlbumEntry	:: PU AlbumEntry
xpAlbumEntry
    = xpElem "entry" $
      xpPair ( xpAttr "path" $
	       xpWrap ( splitPath . mkRelPath, mkAbsPath . joinPath ) $
	       xpText
	     )
	     ( xpWrapPic )

-- ------------------------------------------------------------

instance XmlPickler Archive	where xpickle = xpArchive
instance XmlPickler Config	where xpickle = xpConfig
instance XmlPickler Size        where xpickle = xpSize
instance XmlPickler Geo         where xpickle = xpGeo
instance XmlPickler AspectRatio where xpickle = xpAspectRatio

xpArchive	:: PU Archive
xpArchive
    = xpElem "archive" $
      xpAddFixedAttr "xmlns" "http://muehle.welt.all/photos.dtd" $
      xpWrap ( uncurry Archive
	     , \ a -> (archConfRef a, archRootAlbum a)
	     ) $
      xpPair ( xpDefault "config/archive.xml" $ xpAttr "config" $ xpText )
	     xpAlbumTree

xpConfig	:: PU Config
xpConfig
    = xpElem "config" $
      xpWrap ( \ (x1, x2, x3, x4) -> Config x1 x2 x3 x4
	     , \ c -> (confAttrs c, confLayouts c, confDict c, confSizes c)
	     ) $
      xp4Tuple xpAttrs xpLayouts xpDictionaries xpSizes
	       
xpAttrs		:: PU Attrs
xpAttrs		= xpMap "attr" "name" xpName xpHtmlText

xpHtmlText	:: PU String
xpHtmlText
    = xpWrap ( showXML, readHTML ) $ xpTrees
    where
    showXML  = concat . runLA ( xshow unlistA )
    readHTML = runLA hread				-- hread ignores not wellformed attributes, e.g. unescaped &s in URLs

xpLayouts	:: PU Layouts
xpLayouts	= xpMap "layout" "id" xpName xpLayout

xpLayout	:: PU Layout
xpLayout
    = xpWrap ( uncurry3 Layout
	     , \ l -> (layoutType l, layoutAttrs l, layoutPages l)
	     ) $
      xpTriple ( xpAttr "type" $ xpText )
	       ( xpAttrs )
	       ( xpPages )

xpDictionaries	:: PU Dictionaries
xpDictionaries	= xpMap "dictionary" "id" xpName xpDictionary

xpDictionary	:: PU Dictionary
xpDictionary    = xpAttrs

xpPages		:: PU Pages
xpPages		= xpMap "page" "type" xpName xpAttrs

xpSizes 	:: PU Sizes
xpSizes		= xpList xpSize

xpSize	:: PU Size
xpSize
    = xpElem "size" $
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

xpAspectRatio	:: PU AspectRatio
xpAspectRatio
    = xpWrap ( readAspect
	     , showAspect
	     ) $ xpText

xpGeo	:: PU Geo
xpGeo
    = xpWrap ( readGeo
	     , showGeo
	     ) $ xpText

-- ----------------------------------------

readAspect	:: String -> AspectRatio
readAspect "fix" = Fix
readAspect "pad" = Pad
readAspect _     = Pad

showAspect	:: AspectRatio -> String
showAspect Fix	= "fix"
showAspect Pad  = "pad"

-- ----------------------------------------

readGeo :: String -> Geo
readGeo s
    = Geo (read w) (read h)
    where
    s'      = filter (\ x -> isDigit x || x == 'x') s
    (w, h') = span isDigit s'
    h       = drop 1 h'

showGeo	:: Geo -> String
showGeo g
    = show (geoWidth g) ++ "x" ++ show (geoHeight g)

-- ------------------------------------------------------------

showPic	:: Pic -> String
showPic p
    = concat . intersperse "\n" . filter (not . null) $
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

statusOk	:: Status -> Bool
statusOk (Exc _)	= False
statusOk _		= True

startTr		:: Status -> Status
startTr (Running l)	= Running (l + 1)
startTr e		= e

stopTr		:: Status -> Status
stopTr (Running l)	= Running (l - 1)
stopTr e		= e

-- ------------------------------------------------------------
