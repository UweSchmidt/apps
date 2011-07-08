module Language.Tcl.Core
where

import           Control.Arrow
import           Control.Applicative    ( (<$>) )
import           Control.Monad.Error
import           Control.Monad.RWS

import           Data.Map    		( Map )
import qualified Data.Map      		as M
import           Data.Set    		( Set )
import qualified Data.Set      		as S

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
      { _tglobalVars    :: TclVars
      , _tstack         :: [TclProcFrame]
      , _tcmds          :: TclCommands e s
      , _tprocs         :: TclProcs e s
      , _tchans         :: TclChannels
      , _appState       :: s
      }

instance (Show s) => Show (TclState e s) where
    show (TclState v _s _c _ps ch as)
        = "TclState "
          ++ "{ _tglobalVars = "
          ++ (show . map (second selS) . M.toList $ v)
          -- ++ ", _tcmds = "
          -- ++ (show . M.keys $ c)
          ++ ", _tChans = "
          ++ (show . M.keys $ ch)
          ++ ", _appState = "
          ++ show as
          ++ "}"

type TclWrt		-- not really used
    = String

type TclVars		-- global variables
    = Map String Value

type TclVarSet
    = Set String

data TclProcFrame
    = TPF
      { _tlocals   :: TclVars
      , _tglobals  :: TclVarSet
      , _tprocname :: String
      }
      deriving (Show)

type TclCommands e s	-- commands
    = Map String (TclCommand e s)

type TclCommand e s
    = Values -> TclEval e s Value

type TclProcs e s
    = Map String (TclProc e s)

data TclProc e s
    = TclProc
      { _fparams  :: Value	                  -- the source string of the list of formal param names and default values
      , _fbody    :: Value                        -- the source string of the body
      , _cbody    :: TclEval e s Value	          -- compiled body
      , _cpassing :: Values -> TclEval e s ()     -- compiled param passing
      }

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

evalTcl :: Values -> TclEval e s Value
evalTcl (cn : args)
    = do c <- lookupCmd (selS cn)
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
    = lookupVar n

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

lookupCmd	:: String -> TclEval e s (TclCommand e s)
lookupCmd n
    = get
      >>=
      maybe (tclThrowError $ "invalid command name " ++ show n) return
                . M.lookup n
                . _tcmds

lookupProc :: String -> TclEval e s (TclProc e s)
lookupProc n
    = get
      >>=
      maybe (tclThrowError $ "invalid proc name " ++ show n) return
                . M.lookup n
                . _tprocs

lookupProcOrCmd :: String -> TclEval e s (TclCommand e s)
lookupProcOrCmd n
    = ( buildProcCall <$> lookupProc n )
      `mplus`
      lookupCmd n
    where
      buildProcCall tp
          = \ vs -> pushStackFrame n >> (_cpassing tp) vs >> _cbody tp


commandNames :: TclEval e s [String]
commandNames
    = do cnames <- M.keys . _tcmds <$> get
         pnames <- procNames
         return $ cnames ++ pnames

-- ------------------------------------------------------------

procNames :: TclEval e s [String]
procNames
    = M.keys . _tprocs <$> get

-- ------------------------------------------------------------

pushStackFrame :: String -> TclEval e s ()
pushStackFrame pname
    = modify $
      \ s -> s { _tstack
                     = TPF { _tlocals = M.empty
                           , _tglobals = S.empty
                           , _tprocname = pname
                           }
                       : _tstack s
               }

stackFrameLevel :: TclEval e s Int
stackFrameLevel
    = gets $ length . _tstack

popStackFrame :: TclEval e s ()
popStackFrame
    = do s <- get
         if null . _tstack $ s
            then tclThrowError "proc stack frame underflow"
            else put $
                 s { _tstack = tail . _tstack $ s}

-- ------------------------------------------------------------

lookupLocalVar :: String -> TclEval e s Value
lookupLocalVar n
    = do stack <- _tstack <$> get
         case stack of
           []  -> notFound
           (frame : _)
               -> if n `S.member` _tglobals frame
                  then notFound
                  else case M.lookup n $ _tlocals frame of
                         Nothing -> notFound
                         Just v -> return v
    where
      notFound = tclThrowError $ "can't read local variable " ++ show n ++ ": no such variable"

lookupGlobalVar	:: String -> TclEval e s Value
lookupGlobalVar n
    = do vars <- _tglobalVars <$> get
         case M.lookup n vars of
           Nothing
               -> tclThrowError $ "can't read " ++ show n ++ ": no such variable"
           Just c
               -> return c

lookupVar	:: String -> TclEval e s Value
lookupVar n
    = lookupLocalVar n `mplus` lookupGlobalVar n

-- ------------------------------------------------------------

setLocalVar :: String -> Value -> TclEval e s Value
setLocalVar n v
    = do s <- get
         let stack = _tstack s
         case stack of
           [] -> notFound
           (frame : rest)
               -> if n `S.member` _tglobals frame
                  then notFound
                  else do let vars'  = M.insert n v $ _tlocals frame
                          let frame' = frame { _tlocals = vars' }
                          let stack' = frame' : rest
                          put $ s { _tstack = stack' }
         return v
    where
      notFound = tclThrowError $ "can't write local variable " ++ show n ++ ": no such variable"

setGlobalVar :: String -> Value -> TclEval e s Value
setGlobalVar n v
    = do modify $ \ s -> s { _tglobalVars = M.insert n v (_tglobalVars s) }
         return v

setVar		:: String -> Value -> TclEval e s Value
setVar n v
    = setLocalVar n v `mplus` setGlobalVar n v

-- ------------------------------------------------------------

varName :: String -> TclEval e s Bool
varName n
    = do s <- get
         let vars  = _tglobalVars s
         let stack = _tstack s
         return $
           ( n `M.member` vars )
           ||
           ( (not . null $ stack)
             &&
             n `M.member` (_tlocals . head $ stack)
           )

varNames :: TclEval e s [String]
varNames
    = do globals <- globalVarNames
         locals  <- localVarNames
         return $ locals ++ globals

globalVarNames :: TclEval e s [String]
globalVarNames
    = M.keys . _tglobalVars <$> get

localVarNames :: TclEval e s [String]
localVarNames
    = do stack <- gets _tstack
         return $
           case stack of
             [] -> []
             _  -> M.keys . _tlocals . head $ stack

-- ------------------------------------------------------------

lookupChannel	:: String -> TclState e s -> TclEval e s Handle
lookupChannel n s
    = case M.lookup n $ _tchans s of
        Nothing
            -> tclThrowError $ "can't find channel named " ++ show n
        Just c
            -> return c

-- ------------------------------------------------------------
