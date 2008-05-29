module Album
where

import           Data.Map (Map)
import qualified Data.Map as M
import Data.Tree.NTree.TypeDefs

import System.IO

import Text.XML.HXT.Arrow

import Photo2.ArchiveTypes
import Photo2.FilePath
import Photo2.Arrow

-- import Photo2.State

-- ------------------------------------------------------------

-- test data

g = Geo 20 15
s = Size "dir" g Fix
s2 = Size "xxx" g Pad
l1 = Layout ("html") al1 (M.fromList [("picture", pg1)])
pg1 = M.fromList [("geometry","1600x1200"),("dir","gross")]
al1 = M.fromList [("geometry","1600x1200"),("duration","7000")]
c = Config M.empty (M.fromList [("16x12-css",l1)]) M.empty [s,s2]

-- ----------------------------------------
{-
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
loadAlbum	= loadDocData xpAlbumTree
-}
-- ----------------------------------------

testDir = "http://localhost/~si/praktika/SoftwarePraktikum/photoalbum2/Photoalbum"
testArchive = testDir </> "archive.xml"
testConfig  = testDir </> "config/archive.xml"
testAlbum   = testDir </> "archive/Hagenbeck.xml"

e1 = NTree ab1 [NTree pc1 [], NTree pc2 [], NTree ab2 []]
ab1 = Pic ("myalbum") "" "a.jpg" "" "" M.empty (M.fromList [("title","xxx")]) []
pc1 = Pic ("pic1") "" "p1.jpg" "p1.nef" "p1.xmp" cps1  (M.fromList [("title","picture 1")]) ["errrr"]
pc2 = Pic ("pic2") "" "" "" "" cps1  (M.fromList [("title","picture 1")]) ["no orig found"]
ab2 = Pic ("mysubalbum") "sub.xml" "sub.jpg" "" "" M.empty (M.fromList [("title","external sub")]) []
cps1 = M.fromList [("klein", Copy (Geo 10 10) "klein.jpg")]

-- e2 = head . runLA (addAlbumEntry (["myalbum","mysubalbum"], ( emptyPic {picId = "emil"}))) $ e1

pt = putStrLn . formatAlbumTree

-- ----------------------------------------
{-
sample :: AState ()
sample = do a <- get theConfig
	    io $ print a
	    set (theOption "xxx") "yyy"
	    os <- get theOptions
	    io $ print os
	    o1 <- get (theOption "xxx")
	    io $ print o1
	    update (theOption "xxx") (++ "zzz")
	    os <- get theOptions
	    io $ print os
	    os <- getFrom theOptions (lookup1 "xxx")
	    io $ print os

{-
            update var2 (\x -> x * (fromIntegral a))
            b <- get var2
            io $ print b
	    set var1 42
	    a <- get var1
            io $ print a
	    c <- get sv3
	    io $ print c
	    set sv3 44
	    c <- get sv3
	    io $ print c
	    update sv3 (+1)
	    c <- get sv3
	    io $ print c

-}
{-
main1 = runApp sample

main2 = runApp (processArchive testArchive)

processArchive	:: String -> AState ()
processArchive arname
    = undefined
-}
{-
    = do

      perf "load archive"
	       $ do
		 r <- io $ loadArchive arname
		 if isNothing r
		    then return Nothing
		    else do
			 let ar = fromJust r
			 set theAlbums (archRootAlbum ar)
			 
		 
			    
-}
-}
-- ------------------------------------------------------------

m1 =
    do
    s0 <- runCmd (loadArchiveAndConfig testArchive) initialAppState
    s1 <- runCmd (loadAlbums ["Hagenbeck"]) s0
    -- print s1
    s2 <- runCmd (get theAlbums >>> getAlbumPaths [] >>> arr joinPath >>> arrIO print) s1
    return s2

m2 s2 =
    do
    s3 <- runCmd (loadAllAlbums ["Hagenbeck"]) s2
    s4 <- runCmd (get theAlbums >>> getAlbumPaths [] >>> arr joinPath >>> arrIO print) s3
    return s4

m3 s4 =
    do
    s5 <- runCmd (storeAllAlbums ["Hagenbeck","Zebras"]) s4
    return s5

m =
    do
    s1 <- m1
    s2 <- m2 s1
    s3 <- m3 s2
    return s3