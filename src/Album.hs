module Album
where

import System.IO
import System.FilePath

import Text.XML.HXT.Arrow
import Photo2.Types

import           Data.Map (Map)
import qualified Data.Map as M
import Data.Tree.NTree.TypeDefs

-- ------------------------------------------------------------

-- test data

g = Geo 20 15
s = Size "dir" g Fix
s2 = Size "xxx" g Pad
l1 = Layout "html" al1 (M.fromList [("picture", pg1)])
pg1 = M.fromList [("geometry","1600x1200"),("dir","gross")]
al1 = M.fromList [("geometry","1600x1200"),("duration","7000")]
c = Config M.empty (M.fromList [("16x12-css",l1)]) M.empty [s,s2]

-- ----------------------------------------

loadDocData	:: PU a -> String -> IO (Maybe a)
loadDocData p doc
    = do
      res <- runX $
	     xunpickleDocument p [ (a_remove_whitespace, v_1)
				 , (a_validate, v_0)
				 , (a_tagsoup, v_1)
				 ] doc
	     >>>
	     perform ( xpickleDocument p [ (a_indent, v_1)
					 ] ""
		     )
      if null res
	 then do
	      hPutStrLn stderr $ "loading data from " ++ show doc ++ " failed"
	      return Nothing
	 else return . Just . head $ res

loadArchive	= loadDocData xpArchive
loadConfig	= loadDocData xpConfig
loadAlbum	= loadDocData xpEntry

-- ----------------------------------------

testDir = "http://localhost/~si/praktika/SoftwarePraktikum/photoalbum2/Photoalbum"
testArchive = testDir `combine` "archive.xml"
testConfig  = testDir `combine` "config/archive.xml"
testAlbum   = testDir `combine` "archive/Hagenbeck.xml"

e1 = NTree ab1 [NTree pc1 [], NTree pc2 [], NTree ab2 []]
ab1 = Pic True "myalbum" "" "a.jpg" "" "" M.empty (M.fromList [("title","xxx")]) []
pc1 = Pic False "pic1" "" "p1.jpg" "p1.nef" "p1.xmp" cps1  (M.fromList [("title","picture 1")]) ["errrr"]
pc2 = Pic False "pic1" "" "" "" "" cps1  (M.fromList [("title","picture 1")]) ["no orig found"]
ab2 = Pic True "mysubalbum" "sub.xml" "sub.jpg" "" "" M.empty (M.fromList [("title","external sub")]) []
cps1 = M.fromList [("klein", Copy (Geo 10 10) "klein.jpg")]
