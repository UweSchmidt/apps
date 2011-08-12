module Language.Tcl.Commands.Cd
    ( tclCd
    )
where

import Data.List              ( isPrefixOf )

import Language.Common.Eval   ( liftIOE )

import Language.Tcl.Core
import Language.Tcl.Value

import System.Posix.Directory ( changeWorkingDirectory )
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

changeDir :: String -> IO ()
changeDir
    = changeWorkingDirectory

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
