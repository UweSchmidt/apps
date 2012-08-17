module System.DirTree.Types
    ( module System.DirTree.Types
    , Regex
    )
where

import Control.Monad.RWSErrorIO

import Text.Regex.XMLSchema.String	( Regex
                                        , parseRegex
                                        )

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
    | MatchRE     Regex
    | MatchExtRE  Regex
    | MatchPathRE Regex
    | FTrue
    | FFalse
    | IsFile
    | IsDir
    | HasCont     FindPred
    | AndExpr2    FindExpr FindExpr
    | OrExpr2     FindExpr FindExpr
    | NotExpr     FindExpr

type FindPred = String -> Cmd Bool

-- ----------------------------------------
--
-- smart constructors
-- ----------------------------------------

andExpr :: [FindExpr] -> FindExpr
andExpr = foldr andExpr2 FTrue

andExpr2 :: FindExpr -> FindExpr -> FindExpr
andExpr2 FTrue              e2  = e2
andExpr2 FFalse            _e2  = FFalse
andExpr2 _e1            FFalse  = FFalse
andExpr2 (AndExpr2 e11 e12) e2  = andExpr2 e11 (andExpr2 e12 e2)
andExpr2 e1                 e2  = AndExpr2 e1 e2


orExpr :: [FindExpr] -> FindExpr
orExpr = foldr orExpr2 FTrue

orExpr2 :: FindExpr -> FindExpr -> FindExpr
orExpr2 FFalse              e2  = e2
orExpr2 FTrue              _e2  = FTrue
orExpr2 _e1              FTrue  = FTrue
orExpr2 (OrExpr2 e11 e12)   e2  = orExpr2 e11 (orExpr2 e12 e2)
orExpr2 e1                  e2  = OrExpr2 e1 e2

matchNameRE :: String -> FindExpr
matchNameRE = MatchRE . parseRegex

matchExtRE :: String -> FindExpr
matchExtRE = MatchExtRE . parseRegex

matchPathRE :: String -> FindExpr
matchPathRE = MatchPathRE . parseRegex

-- ----------------------------------------
