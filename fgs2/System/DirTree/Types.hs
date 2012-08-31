module System.DirTree.Types
    ( module System.DirTree.Types
    , Regex
    )
where

import Control.Monad.RWSErrorIO

import Text.Regex.XMLSchema.String      ( Regex
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
      { theProgName      :: String
      , theRootDir       :: String
      , theCwd           :: String
      , theLevel         :: Int
      , theUserFindExpr  :: FindExpr
      , theSysFindExpr   :: FindExpr
      , theDirFindExpr   :: FindExpr
      , theFindPred      :: FindPred
      , theDirPred       :: FindPred
      , theGrepPred      :: String -> Bool
      , theSedFct        :: String -> String
      , theProcessor     :: Cmd (Cmd (), String -> Cmd (), Cmd ())
      , theFollowSymlink :: Bool
      , theTraceFlag     :: Bool
      , theWarningFlag   :: Bool
      , theErrorFlag     :: Bool
      , theStdErrFlag    :: Bool
      , theUtf8DecFlag   :: Bool
      , theUtf8EncFlag   :: Bool
      , theCreateBackup  :: Bool
      , theBackupName    :: String -> String
      , theHashUpdate    :: Bool
      , theHashFct       :: HashFct
      , theChecksumFile  :: String
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
    | HasType     EntryType
    | HasFeature  String   FindPred
    | HasCont     String   FindPred
    | AndExpr     FindExpr FindExpr
    | OrExpr      FindExpr FindExpr
    | NotExpr     FindExpr

data EntryType = IsFile | IsDir | IsSymLink | IsCharDev | IsBlockDev | IsSocket | IsNamedPipe
                 deriving (Eq)

type FindPred = String -> Cmd Bool

-- ------------------------------

fCost :: FindExpr -> Int
fCost (FTrue         ) = 0
fCost (FFalse        ) = 0
fCost (MatchRE      _) = 1
fCost (MatchExtRE   _) = 1
fCost (MatchPathRE  _) = 1
fCost (HasType      _) = 2
fCost (HasFeature _ _) = 3
fCost (HasCont    _ _) = 4
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

andExpr (AndExpr  e11 e12) e2  = andExpr e11 (andExpr e12 e2)  -- Assoc

andExpr e1  (AndExpr e21 e22)
    | fCost e21 < fCost e1     = andExpr e21 (andExpr e1 e22)  -- Symmetry

andExpr e1                 e2
    | fCost e2  < fCost e1     = andExpr e2 e1                 -- Symmetry
    | otherwise                = AndExpr e1 e2

orExprSeq :: [FindExpr] -> FindExpr
orExprSeq = foldr orExpr FFalse

orExpr :: FindExpr -> FindExpr -> FindExpr
orExpr FFalse              e2  = e2
orExpr FTrue              _e2  = FTrue
orExpr _e1              FTrue  = FTrue
orExpr (OrExpr  e11 e12)   e2  = orExpr e11 (orExpr e12 e2)

orExpr e1  (OrExpr e21 e22)
    | fCost e21 < fCost e1     = orExpr e21 (orExpr e1 e22)  -- Symmetry
orExpr e1                  e2
    | fCost e2  < fCost e1     = orExpr e2 e1                -- Symmetry
    | otherwise                = OrExpr e1 e2

matchNameRE :: String -> FindExpr
matchNameRE = MatchRE . parseRegex

matchExtRE :: String -> FindExpr
matchExtRE = MatchExtRE . parseRegex

matchPathRE :: String -> FindExpr
matchPathRE = MatchPathRE . parseRegex

-- ----------------------------------------
