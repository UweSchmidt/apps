module System.DirTree.Types
    ( module System.DirTree.Types
    , Regex
    )
where

import Control.Monad.RWSErrorIO

import Text.Regex.XMLSchema.String	( Regex
                                        , parseRegex
                                        )

import qualified Data.ByteString        as B

-- ----------------------------------------

type Hash        = String

type HashFct     = ByteString -> Hash

type ByteString  = B.ByteString

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
      , theErrorFlag    :: Bool
      , theStdErrFlag   :: Bool
      , theUtf8DecFlag  :: Bool
      , theUtf8EncFlag  :: Bool
      , theCreateBackup :: Bool
      , theBackupName   :: String -> String
      , theHashUpdate   :: Bool
      , theHashFct      :: HashFct
      , theChecksumFile :: String
      }

instance Config Env where
    traceOn   = theTraceFlag
    warningOn = theWarningFlag
    errorOn   = theErrorFlag
    stderrOn  = theStdErrFlag


-- ----------------------------------------

type State = ()

-- ----------------------------------------

type Cmd = Action Env State

-- ----------------------------------------

data FindExpr
    = MatchRE     Regex
    | MatchExtRE  Regex
    | MatchPathRE Regex
    | FTrue
    | FFalse
    | IsFile
    | IsDir
    | HasCont     FindPred
    | AndExpr     FindExpr FindExpr
    | OrExpr      FindExpr FindExpr
    | NotExpr     FindExpr

type FindPred = String -> Cmd Bool

-- ------------------------------

fCost :: FindExpr -> Int
fCost (FTrue         ) = 0
fCost (FFalse        ) = 0
fCost (MatchRE      _) = 1
fCost (MatchExtRE   _) = 1
fCost (MatchPathRE  _) = 1
fCost (IsFile        ) = 2
fCost (IsDir         ) = 2
fCost (HasCont     _ ) = 3
fCost (AndExpr  e1 e2) = fCost e1 `max` fCost e2
fCost (OrExpr   e1 e2) = fCost e1 `max` fCost e2
fCost (NotExpr  e1   ) = fCost e1

-- ----------------------------------------
--
-- smart constructors
-- ----------------------------------------

andExprSeq :: [FindExpr] -> FindExpr
andExprSeq = foldr andExpr FTrue

andExpr :: FindExpr -> FindExpr -> FindExpr
andExpr FTrue              e2  = e2
andExpr FFalse            _e2  = FFalse
andExpr _e1            FFalse  = FFalse
andExpr (AndExpr  e11 e12) e2  = andExpr e11 (andExpr e12 e2)
andExpr e1                 e2
    | fCost e1 <= fCost e2      = AndExpr e1 e2
    | otherwise                 = andExpr e2 e1

orExprSeq :: [FindExpr] -> FindExpr
orExprSeq = foldr orExpr FFalse

orExpr :: FindExpr -> FindExpr -> FindExpr
orExpr FFalse              e2  = e2
orExpr FTrue              _e2  = FTrue
orExpr _e1              FTrue  = FTrue
orExpr (OrExpr  e11 e12)   e2  = orExpr e11 (orExpr e12 e2)
orExpr e1                  e2
    | fCost e1 <= fCost e2     = OrExpr e1 e2
    | otherwise                = orExpr e2 e1

matchNameRE :: String -> FindExpr
matchNameRE = MatchRE . parseRegex

matchExtRE :: String -> FindExpr
matchExtRE = MatchExtRE . parseRegex

matchPathRE :: String -> FindExpr
matchPathRE = MatchPathRE . parseRegex

-- ----------------------------------------
