module Language.Tcl.Core
where

import           Control.Arrow
import           Control.Monad.Error
import           Control.Monad.RWS

import           Data.Map    		( Map )
import qualified Data.Map      		as M

import           Language.Common.Eval

import           Language.Tcl.Value
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
          ++ (show . map (second selS) . M.toList $ v)
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
    = Map String Value

type TclCommands e s	-- commands
    = Map String (TclCommand e s)

type TclCommand e s
    = Values -> TclEval e s Value

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

tclCatch :: (Int -> Bool) -> TclEval e s Value -> TclEval e s Value
tclCatch
    = tclTryCatch (\ (TclError _lev msg) -> return (mkS msg))

tclChangeErr :: String -> (Int -> Bool) -> TclEval e s Value -> TclEval e s Value
tclChangeErr msg
    = tclTryCatch (\ (TclError lev _msg) -> throwError (tclOtherExc lev msg))

tclTryCatch :: (TclError -> TclEval e s Value) -> (Int -> Bool) -> TclEval e s Value -> TclEval e s Value
tclTryCatch handler p cmd
    = cmd
      `catchError`
      (\ err@(TclError lev _msg)
           -> if p lev
              then handler    err
              else throwError err
      )

tclCatchError :: TclEval e s Value -> TclEval e s Value
tclCatchError
    = tclCatch (== 1)

tclCatchReturnExc :: TclEval e s Value -> TclEval e s Value
tclCatchReturnExc
    = tclCatch (== 2)

tclCatchBreakExc :: TclEval e s Value -> TclEval e s Value
tclCatchBreakExc
    = tclCatch (== 3)

tclCatchContinueExc :: TclEval e s Value -> TclEval e s Value
tclCatchContinueExc
    = tclCatch (== 4)

tclFromEither :: Either String r -> TclEval e s r
tclFromEither
    = either tclThrowError return

-- ------------------------------------------------------------

interpreteTcl	:: String -> TclEval e s Value
interpreteTcl s
    = parseTclProg s >>= evalTclProg

parseTclProg	:: String -> TclEval e s TclProg
parseTclProg s
    = case (P.parseTclProg s) of
        Left err
            -> tclThrowError $ show err
        Right p
            -> return p

evalTclProg	:: TclProg -> TclEval e s Value
evalTclProg (TclProg tp)
    | null tp
        = return value_empty
    | otherwise
        = do l <- mapM evalTclCmd tp
             return (last l)

-- ------------------------------------------------------------

evalTclCmd	:: TclCmd -> TclEval e s Value
evalTclCmd (TclCmd al)
    = mapM evalTclArg al >>= evalTcl
    where
      evalTcl :: Values -> TclEval e s Value
      evalTcl (cn : args)
          = do s <- get
               c <- lookupCmd (selS cn) s
               c args
      evalTcl []
          = tclThrowError "empty command"

evalTclArg	:: TclArg -> TclEval e s Value
evalTclArg (TclArg xs)
    = mapM evalTclSubst xs >>= return . mconcat

evalTclSubst	:: TclSubst -> TclEval e s Value
evalTclSubst (TLit s)
    = return $ mkS s

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

evalTclL :: TclList -> TclEval e s Values
evalTclL (TclList al)
    = mapM evalTclArg al

evalTclList :: String -> TclEval e s Values
evalTclList s
    = parseTclList s >>= evalTclL
 
-- ------------------------------------------------------------

parseTclArgs	:: String -> TclEval e s TclCmd
parseTclArgs s
    = case (P.parseTclArgs s) of
        Left err
            -> tclThrowError $ show err
        Right l
            -> return l

substTclArgs :: TclCmd -> TclEval e s Values
substTclArgs (TclCmd al)
    = mapM evalTclArg al

evalTclArgs :: String -> TclEval e s Values
evalTclArgs s
    = parseTclArgs s >>= substTclArgs
 
-- ------------------------------------------------------------

lookupCmd	:: String -> TclState e s -> TclEval e s (TclCommand e s)
lookupCmd n s
    = case M.lookup n $ _tcmds s of
        Nothing
            -> tclThrowError $ "invalid command name " ++ show n
        Just c
            -> return c

lookupVar	:: String -> TclState e s -> TclEval e s Value
lookupVar n s
    = case M.lookup n $ _tvars s of
        Nothing
            -> tclThrowError $ "can't read " ++ show n ++ ": no such variable"
        Just c
            -> return c

setVar		:: String -> Value -> TclState e s -> TclEval e s Value
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
