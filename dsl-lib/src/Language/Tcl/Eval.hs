module Language.Tcl.Eval
where

import Control.Monad.Error
import Control.Monad.RWS

import           Data.Map     		( Map )
import qualified Data.Map               as M

import Language.Common.Eval

import Language.Tcl.AbstractSyntax
import Language.Tcl.Parser
import Language.Tcl.Show

import System.IO

-- ------------------------------------------------------------

data TclEnv e
    = TclEnv
      { _appEnv :: e
      }

data TclState e s
    = TclState
      { _tvars    :: TclVars
      , _tcmds    :: TclCommands e s
      , _tchans   :: TclChannels
      , _appState :: s
      }

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

type TclEval e s
    = Eval (TclEnv e) TclWrt (TclState e s)

-- ------------------------------------------------------------

initTclEnv	:: e -> TclEnv e
initTclEnv e
    = TclEnv
      { _appEnv = e
      }

initTclState :: s -> TclState e s
initTclState s
    = TclState
      { _tvars    = M.empty
      , _tcmds    = M.fromList buildInTclCommands
      , _tchans   = M.fromList buildInTclChannels
      , _appState = s
      }

buildInTclCommands :: [(String, TclCommand e s)]
buildInTclCommands
    = [ ("puts", 	tclPuts)
      , ("set",		tclSet)
      ]

buildInTclChannels :: [(String, Handle)]
buildInTclChannels
    = [ ("stdout", stdout)
      , ("stdin",   stdin)
      , ("stderr", stderr)
      ]

-- ------------------------------------------------------------

evalTclProg	:: TclProg -> TclEval e s String
evalTclProg (TclProg tp)
    | null tp
        = return ""
    | otherwise
        = do l <- mapM evalTclCmd tp
             return (last l)

evalTclCmd	:: TclCmd -> TclEval e s String
evalTclCmd (TclCmd al)
    = mapM evalTclArg al >>= evalTcl

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

parseTcl	:: String -> TclEval e s TclProg
parseTcl s
    = case (parseTclProg s) of
        Left err
            -> throwError $ show err
        Right p
            -> return p

evalTcl		:: [String] -> TclEval e s String
evalTcl (cn : args)
    = do s <- get
         c <- lookupCmd cn s
         c args
evalTcl []
    = throwError "empty command"

interpreteTcl	:: String -> TclEval e s String
interpreteTcl s
    = parseTcl s >>= evalTclProg

-- ------------------------------------------------------------

lookupCmd	:: String -> TclState e s -> TclEval e s (TclCommand e s)
lookupCmd n s
    = case M.lookup n $ _tcmds s of
        Nothing
            -> throwError $ "invalid command name " ++ show n
        Just c
            -> return c

lookupVar	:: String -> TclState e s -> TclEval e s String
lookupVar n s
    = case M.lookup n $ _tvars s of
        Nothing
            -> throwError $ "can't read " ++ show n ++ ": no such variable"
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
            -> throwError $ "can't find channel named " ++ show n
        Just c
            -> return c

-- ------------------------------------------------------------

tclSet	:: TclCommand e s
tclSet [n]
    = get >>= lookupVar n

tclSet [n, v]
    = get >>= setVar n v

tclSet _
    = throwError "wrong # args: should be \"set varName ?newValue?\""

-- ------------------------------------------------------------

tclPuts :: TclCommand e s
tclPuts l
    = uncurry tclPuts' (tclOption "-nonewline" l)
      >> return ""
      where
        tclPuts' nnl [s]
            = tclPuts' nnl ["stdout", s]

        tclPuts' nnl [cn, s]
            = do h <- get >>= lookupChannel cn
                 liftIOE $ ( if nnl
                             then hPutStr
                             else hPutStrLn) h s

        tclPuts' _ _
            = throwError "wrong # args: should be \"puts ?-nonewline? ?channelId? string\""

-- ------------------------------------------------------------

tclOption :: String -> [String] -> (Bool, [String])
tclOption n (x : xs)
    | n == x
	= (True, xs)

tclOption _ xs
    = (False, xs)

-- ------------------------------------------------------------
