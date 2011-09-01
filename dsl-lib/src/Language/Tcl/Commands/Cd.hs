module Language.Tcl.Commands.Cd
{-
    ( tclCd
    , tclPwd
    , tclDirContents
    , tclFile
    , tclGlob
    )
-}
where

import Control.Applicative    ( (<$>) )
import Control.Arrow          ( first
                              , second
                              )
import Control.Monad          ( filterM )

import Data.List              ( isPrefixOf )
import Data.Maybe             ( fromJust )

import Language.Common.Eval   ( liftIOE )
import Language.Common.EvalOptions

import Language.Tcl.Core
import Language.Tcl.Value


import System.Directory       ( setCurrentDirectory
                              , getCurrentDirectory
                              , getDirectoryContents
                              , doesDirectoryExist
                              )
import System.FilePath        -- ( (</>) )
{-
import System.Posix.Directory ( changeWorkingDirectory
                              , getWorkingDirectory
                              )
-}
import System.Posix.User      ( getRealUserID
			      , getUserEntryForID
			      , getUserEntryForName
			      , homeDirectory
			      )

-- ------------------------------------------------------------

tclCd :: TclCommand e s
tclCd (path' : [])
    | "~" `isPrefixOf` path
	= do path1 <- liftIOE $ tildeSubst path
	     tclCd [mkS path1]
    | otherwise
	= (liftIOE $ changeDir path) >> return mempty
    where
      path = selS path'

tclCd []
    = do hd <- liftIOE getHomeDir
         tclCd [mkS hd]

tclCd _
    = tclWrongArgs "cd ?dirName?"

-- ------------------------------------------------------------

tclPwd :: TclCommand e s
tclPwd []
    = liftIOE getCurrentDirectory
      >>= return . mkS

tclPwd _
    = tclWrongArgs "pwd"

-- ------------------------------------------------------------

tclFile :: TclCommand e s
tclFile _
    = tclWrongArgs "file option ?arg ...?"

-- ------------------------------------------------------------

tclGlob :: TclCommand e s
tclGlob l0
    = do (go, l) <- tclFromEither . evalOptions globOptions globDefaults $ l0
         glob go l
    where
      glob _ []
          = tclWrongArgs "glob ?switches? name ?name ...?"

      glob (complain, (join, (px, ()))) l'
          | join			-- build a path from the list of patterns
              = glob1 prefix p
                >>= complain p
          | otherwise			-- evaluate all pattern and concat the result
              = (concat . map (fromJust . selL) . concat) <$> sequence (map (glob1 prefix) l)
                >>= complain p
          where
            prefix = escapeGlobPattern px
            p      = joinPath l
            l      = map selS l'

      glob1 prefix pat
          = do res <- liftIOE $ dirContents base fs dir
               return (map mkS res)
            where
              base      = ""
              (dir, fs) = pat2Filter . splitDirectories $ (prefix ++ pat)
            
globDefaults :: (String -> TclCommand e s, (Bool, (String, ())))
globDefaults
    = (globComplain, (False, ("", ())))

globOptions :: OptParser [Value] (String -> TclCommand e s, (Bool, (String, ())))
globOptions
    = optionsUntil   (isOpt ((== "--")   . selS) id)
      [ isOpt        ((== "-nocomplain") . selS) ( first  $ const globNoComplain )
      , isOpt        ((== "-join")       . selS) ( second . first $ const True   )
      , isArgOpt     ((== "-path")       . selS) ( \ v ->
                                                   second . second . first $ const (selS v) )
      , isIllegalOpt (("-" `isPrefixOf`) . selS) "must be -join, -nocomplain or --"
      ]

pat2Filter :: [String] -> (String, [String -> Bool])
pat2Filter ("/" : pl)
    = ("/", p2f pl)
pat2Filter pl
    = (".", p2f pl)

p2f :: [String] -> [String -> Bool]
p2f = map name2pred
    where
      name2pred name
          | "." `isPrefixOf` name
              = prd
          | otherwise
              = \ s -> not ("." `isPrefixOf` s) && prd s
          where
            prd = matchGlobPattern name

globComplain :: String -> TclCommand e s
globComplain pat l
    | null l
        = tclThrowError $ "no files matched glob pattern " ++ show pat
    | otherwise
        = return (mkL l)

globNoComplain :: String -> TclCommand e s
globNoComplain _ l
    = return (mkL l)

-- ------------------------------------------------------------

tclDirContents :: TclCommand e s
tclDirContents (path : [])
    = (mkL . map mkS) <$> liftIOE (getDirectoryContents $ selS path)

tclDirContents _
    = tclWrongArgs "tcl_dirContents ?dirName?"

-- ------------------------------------------------------------

changeDir :: String -> IO ()
changeDir
    = setCurrentDirectory

getHomeDir :: IO String
getHomeDir
    = do uid <- getRealUserID
	 uen <- getUserEntryForID uid
	 return $ homeDirectory uen

getHomeDir' :: String -> IO String
getHomeDir' user
    = do uen <- getUserEntryForName user
	 return $ homeDirectory uen

tildeSubst :: String -> IO String
tildeSubst path0
    = do hd <- if null user then getHomeDir else getHomeDir' $ user
	 return $ hd ++ path
    where
    (user, path) = break (== '/') . drop 1 $ path0

-- ------------------------------------------------------------

dirContent :: String -> (String -> Bool) -> String -> IO [String]
dirContent base prd dir
    = filter prd <$> getDirectoryContents (base </> dir)

dirContents :: String -> [String -> Bool] -> String -> IO [String]
dirContents base [] dir
    = do x <- doesDirectoryExist (base </> dir)
         return $ if x then [dir] else []
      
dirContents base [prd] dir
    = map (dir </>) <$> dirContent base prd dir

dirContents base (prd1 : prds) dir
    = do entries    <- map (dir </>) <$> dirContent base prd1 dir
         subdirs    <- filterM doesDirectoryExist entries
         subentries <- sequence $ map (dirContents base prds) subdirs
         return $ concat subentries

-- ------------------------------------------------------------
