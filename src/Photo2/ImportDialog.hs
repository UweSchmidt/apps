module Photo2.ImportDialog
where

import qualified Control.Monad as CM

import           Data.Atom
import           Data.List               ( isPrefixOf
					 , isInfixOf
					 )
import qualified Data.Map as M
import           Data.Maybe

import           Photo2.ArchiveTypes
import           Photo2.Arrow
import           Photo2.ExifData
import           Photo2.FilePath
import           Photo2.ImageOperations
import           Photo2.SearchNewImages

import           System.Console.Readline ( readline
					 , addHistory
					 )
import           System.IO

import           Text.XML.HXT.Arrow

-- ------------------------------------------------------------

importPics	:: ConfigArrow AlbumTree AlbumTree
importPics c p
    = runAction ("importing pictures into " ++ showPath p)
      ( checkEntryLoaded
	>>>
	(ins $< (importScan >>> importSelection))
      )
    where
    importScan
	= runAction ("scan for pictures to be imported")
	  (get theConfig >>> arrIOE scanForNewImages)

    importSelection
	= runAction ("import selection dialog")
	  (arrIOE $ importDialog c)

    ins nps
	= runAction ( "import dialog for pictures\n"
		      ++
		      concatMap (\ (o,_r,_x,a) -> "\t" ++ o ++ (show . M.toList $ a) ++ "\n") nps
		    )
	  ( replaceChildren ( getChildren
			      <+>
			      ((getChildren >>> getPicId) >>. newPics)
			    )
	  )
	  where
	  newPics	:: [String] -> [AlbumTree]
	  newPics used
	    = zipWith newPic newIds nps
	    where
	    newIds	:: [String]
	    newIds 	= filter (`notElem` used) . map picnr $ [1..]

	    newPic	:: String -> PicDescr -> AlbumTree
	    newPic pid (o,r,x,al)
			= albumTree $
			  emptyPic { picId	= pid
				   , picOrig	= o
				   , picRaw	= r
				   , picXmp	= x
				   , picAttrs	= al
				   }


-- ------------------------------------------------------------

type PicDescr	= (String, String, String, Attrs)
type PicList	= [PicDescr]

importDialog	:: Config -> [(String, String, String)] -> IOE PicList
importDialog cnf pics
    = do
      viewPics
      liftIO $ dialog emptyAttrs [] [] pics'
    where
    imgBase	= normPath . getImportBase $ cnf
    pics'	= map (\ (x1, x2, x3) -> (x1, x2, x3, emptyAttrs)) pics

    viewPics
	= exec ( "gwenview -" ++ "-geometry 1600x800+0+0 "
		 ++
		 concatMap (picpath . (\(x,_,_) -> x)) pics
	         ++
		 " 2> /dev/null &"
		 )
	where
	picpath = (' ':) . (imgBase </>)

    dialog _env ss _ns []
	= return $ reverse ss
    dialog env ss ns rs@(p@(n, _r, _x, _al) : rs1)
	= readCmd
	where
	fn = imgBase </> n
	usage
	    = unlines $
	      [ "+\t\timport picture"
	      , "-\t\tskip picture"
	      , "q\t\tquit import"
	      , "a\t\ttake remaining pictures and quit"
	      , "> <search>\tmove forward in import list until seach is found in filename"
	      , "< <search>\tmove backward in list of not imported until seach is found"
	      , "t <title>\tgive one line title, \"t\" clears"
	      , "s <subtitle>\tgive one line subtitle, \"s\" clears subtitle"
	      , "k <keywords>\tgive keywords, whitespace separated, \"k\" clears list of keywords"
	      , "c <comment>\tgive comment, \"c\" clears comment"
	      , "d <description>\tgive descripion, \"d\" clears description"
	      , "r <resource>\tgive further resources, \"r\" clears resource"
	      , "h,?\t\tthis message"
	      ]
	readCmd
	    = do
	      line <- readline ("import " ++ fn ++ " [+-><aq?tskch] : ")
	      let line' = stringTrim . fromMaybe "q" $ line
	      CM.when (length line' > 1) (addHistory line')
	      if null line'
		 then readCmd
		 else evalCmd line'
	evalCmd cmd
	    | c == 'q'
		= dialog env ss ns []
	    | c == '-'
		= dialog env ss (p : ns) rs1
	    | c == '+'
		= dialog env (addEnv p : ss) ns rs1
	    | c == '>'
		= let
		  (ns', rs') = searchForward (p:ns) rs1
		  in
		  dialog env ss ns' rs'
	    | c == '<'
		= let
		  (ns', rs') = searchBackward ns rs
		  in
		  dialog env ss ns' rs'
	    | c == 'a'
		= dialog env (foldl (\ l e -> addEnv e : l) ss rs) ns []
	    | c `elem` inames
		= dialog (M.insert (head . filter (([c] `isPrefixOf`) . show) $ keys) v' env) ss ns rs
	    | c `elem` "h?"
		= do
		  hPutStrLn stdout usage
		  readCmd
	    | otherwise
		= do
		  hPutStrLn stdout ("unknown command " ++ show c)
		  hPutStrLn stdout usage
		  readCmd
	    where
	    c	= head cmd
	    v   = stringTrim $ tail cmd
	    v'	= concat . runLA (xshow hread) $ v	-- substitute HTML entity refs, e.g. &oslash;
	    names  = ["title", "subtitle", "keywords", "comment", "descripion", "resource"]
	    inames = map head names
            keys   = map newAtom names

	    addEnv (x1, x2, x3, e)
		= (x1, x2, x3, normAttrs (confPicAttrs cnf) $ env `M.union` e)

	    searchForward xs []
		= (xs, [])
	    searchForward xs ys@((nm,_,_,_) : ys1)
		| v `isInfixOf` nm
		    = (xs, ys)
		| otherwise
		    = searchForward (p : xs) ys1

	    searchBackward [] ys
		= ([], ys)
	    searchBackward (x@(nm,_,_,_) : xs1) ys
		| v `isInfixOf` nm
		    = (xs1, x : ys)
		| otherwise
		    = searchBackward xs1 (x : ys)

-- ------------------------------------------------------------
