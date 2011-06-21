module Language.Tcl.Core
where

import           Control.Monad.Error
import           Control.Monad.RWS

import           Data.Map    		( Map )
import qualified Data.Map      		as M

import           Language.Common.Eval

import           Language.Tcl.AbstractSyntax
import qualified Language.Tcl.Parser    as P

import           System.IO

-- ------------------------------------------------------------

data TclEnv e
    = TclEnv
      { _appEnv :: e
      }
      deriving (Show)

data TclState e s
    = TclState
      { _tvars    :: TclVars
      , _tcmds    :: TclCommands e s
      , _tchans   :: TclChannels
      , _appState :: s
      }

instance (Show s) => Show (TclState e s) where
    show (TclState v c ch as)
        = "TclState "
          ++ "{ _tvars = "
          ++ show v
          ++ ", _tcmds = "
          ++ (show . M.keys $ c)
          ++ ", _tChans = "
          ++ (show . M.keys $ ch)
          ++ ", _appState = "
          ++ show as
          ++ "}"

type TclWrt		-- not really used
    = String

type TclVars		-- global variables
    = Map String String

type TclCommands e s	-- commands
    = Map String (TclCommand e s)

type TclCommand e s
    = [String] -> TclEval e s String

type TclChannels	-- open channels
    = Map String Handle

data TclError
    = TclError
      { _tclErrorLevel :: Int
      , _tclErrorMsg   :: String
      }
      deriving (Show)

type TclEval e s
    = Eval TclError (TclEnv e) TclWrt (TclState e s)

-- ------------------------------------------------------------

instance Error TclError where
    noMsg  = tclErrorExc "unknonw Tcl error"
    strMsg = tclErrorExc 

tclErrorExc :: String -> TclError
tclErrorExc
    = TclError 1

tclReturnExc :: String -> TclError
tclReturnExc
    = TclError 2

tclBreakExc :: TclError
tclBreakExc
    = TclError 3 ""

tclContinueExc :: TclError
tclContinueExc
    = TclError 4 ""

tclOtherExc :: Int -> String -> TclError
tclOtherExc
    = TclError

tclThrowError :: String -> TclEval e s r
tclThrowError
    = throwError . tclErrorExc

tclWrongArgs :: String -> TclEval e s r
tclWrongArgs
    = tclThrowError . ("wrong # args: should be " ++) . show

tclCatch :: (Int -> Bool) -> TclEval e s String -> TclEval e s String
tclCatch p cmd
    = cmd
      `catchError`
      (\ err@(TclError lev msg)
           -> if p lev
              then return msg
              else throwError err
      )

tclCatchError :: TclEval e s String -> TclEval e s String
tclCatchError
    = tclCatch (== 1)

tclCatchReturnExc :: TclEval e s String -> TclEval e s String
tclCatchReturnExc
    = tclCatch (== 2)

tclCatchBreakExc :: TclEval e s String -> TclEval e s String
tclCatchBreakExc
    = tclCatch (== 3)

tclCatchContinueExc :: TclEval e s String -> TclEval e s String
tclCatchContinueExc
    = tclCatch (== 4)

-- ------------------------------------------------------------

interpreteTcl	:: String -> TclEval e s String
interpreteTcl s
    = parseTclProg s >>= evalTclProg

parseTclProg	:: String -> TclEval e s TclProg
parseTclProg s
    = case (P.parseTclProg s) of
        Left err
            -> tclThrowError $ show err
        Right p
            -> return p

evalTclProg	:: TclProg -> TclEval e s String
evalTclProg (TclProg tp)
    | null tp
        = return ""
    | otherwise
        = do l <- mapM evalTclCmd tp
             return (last l)

-- ------------------------------------------------------------

evalTclCmd	:: TclCmd -> TclEval e s String
evalTclCmd (TclCmd al)
    = mapM evalTclArg al >>= evalTcl
    where
      evalTcl :: [String] -> TclEval e s String
      evalTcl (cn : args)
          = do s <- get
               c <- lookupCmd cn s
               c args
      evalTcl []
          = tclThrowError "empty command"

evalTclArg	:: TclArg -> TclEval e s String
evalTclArg (TclArg xs)
    = mapM evalTclSubst xs >>= return . concat

evalTclSubst	:: TclSubst -> TclEval e s String
evalTclSubst (TLit s)
    = return s

evalTclSubst (TVar n)
    = get >>= lookupVar n

evalTclSubst (TEval p)
    = evalTclProg p

-- ------------------------------------------------------------

parseTclList	:: String -> TclEval e s TclList
parseTclList s
    = case (P.parseTclList s) of
        Left err
            -> tclThrowError $ show err
        Right l
            -> return l

evalTclL :: TclList -> TclEval e s [String]
evalTclL (TclList al)
    = mapM evalTclArg al

evalTclList :: String -> TclEval e s [String]
evalTclList s
    = parseTclList s >>= evalTclL
 
-- ------------------------------------------------------------

lookupCmd	:: String -> TclState e s -> TclEval e s (TclCommand e s)
lookupCmd n s
    = case M.lookup n $ _tcmds s of
        Nothing
            -> tclThrowError $ "invalid command name " ++ show n
        Just c
            -> return c

lookupVar	:: String -> TclState e s -> TclEval e s String
lookupVar n s
    = case M.lookup n $ _tvars s of
        Nothing
            -> tclThrowError $ "can't read " ++ show n ++ ": no such variable"
        Just c
            -> return c

setVar		:: String -> String -> TclState e s -> TclEval e s String
setVar n v s
    = do put $ s { _tvars = M.insert n v (_tvars s) }
         return v

lookupChannel	:: String -> TclState e s -> TclEval e s Handle
lookupChannel n s
    = case M.lookup n $ _tchans s of
        Nothing
            -> tclThrowError $ "can't find channel named " ++ show n
        Just c
            -> return c

-- ------------------------------------------------------------
