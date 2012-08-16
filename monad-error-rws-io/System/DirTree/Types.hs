module System.DirTree.Types
where

import Control.Monad.RWSErrorIO

-- ----------------------------------------

data Env
    = Env
      { theProgName     :: String
      , theRootDir      :: String
      , theCwd          :: String
      , theLevel        :: Int
      , theUserFindExpr :: FindExpr
      , theSysFindExpr  :: FindExpr
      , theFindPred     :: FindPred
      , theGrepPred     :: String -> Bool
      , theSedFct       :: String -> String
      , theProcessor    :: Cmd (Cmd (), String -> Cmd (), Cmd ())
      , theTraceFlag    :: Bool
      , theWarningFlag  :: Bool
      , theStdErrFlag   :: Bool
      , theCreateBackup :: Bool
      , theBackupName   :: String -> String
      }

instance Config Env where
    traceOn   = theTraceFlag
    warningOn = theWarningFlag
    stderrOn  = theStdErrFlag


-- ----------------------------------------

type State = ()

-- ----------------------------------------

type Cmd = Action Env State

-- ----------------------------------------

data FindExpr
    = FPred       FindPred
    | Ext         String
    | Name        String
    | PathName    String
    | MatchRE     String
    | MatchExtRE  String
    | MatchPathRE String
    | FTrue
    | FFalse
    | IsFile
    | IsDir
    | HasCont     FindPred
    | AndExpr    [FindExpr]
    | OrExpr     [FindExpr]
    | NotExpr     FindExpr

type FindPred = String -> Cmd Bool

-- ----------------------------------------
