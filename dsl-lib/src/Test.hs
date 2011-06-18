{-# OPTIONS -XGeneralizedNewtypeDeriving #-}

module Test
where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Trans

import System.IO

-- ------------------------------------------------------------

type Eval env wrt st
    = ErrorT String (RWST env wrt st IO)

runEval :: Eval env wrt st res -> env -> st -> IO (Either String res, st, wrt)
runEval expr env st 
    = runRWST (runErrorT expr) env st

-- ------------------------------------------------------------

type Cmd expr env wrt st
    = ReaderT (CmdEnv expr env wrt st) (StateT (CmdState st) IO)

data CmdState st
    = CmdState
      { _exprState :: st
      , _cmdCnt    :: Int
      }

data CmdEnv expr env wrt st
    = CmdEnv
      { _exprEnv  :: env
      , _hello    :: Cmd expr env wrt st String
      , _bye      :: Cmd expr env wrt st String
      , _prompt   :: Cmd expr env wrt st String
      , _prompt2  :: Cmd expr env wrt st String
      , _batch    :: Cmd expr env wrt st Bool
      , _read     :: String -> Cmd expr env wrt st (Maybe String)
      , _parse    :: String -> Cmd expr env wrt st (Command expr)
      , _eval     :: expr -> env -> st -> IO (Either String String, st, wrt)
      }

data Command expr
    = CNoop
    | CIncomplete String
    | CErr  String
    | CExpr expr

runCommandLoop :: (Show wrt) => CmdEnv expr env wrt st -> CmdState st -> IO ()
runCommandLoop initEnv initState
    = execStateT (runReaderT cmdLoopInit initEnv) initState >> return ()

-- ------------------------------------------------------------

cmdLoopInit :: (Show wrt) => Cmd expr env wrt st ()
cmdLoopInit
    = do msg  <- ask >>= _hello
         whenInteractive $ putErr msg
         cmdLoop

cmdLoop :: (Show wrt) => Cmd expr env wrt st ()
cmdLoop
    = do cenv   <- ask
         prompt <- _prompt cenv
         line   <- _read   cenv prompt
         maybe cmdLoopExit cmdParse line

cmdParse :: (Show wrt) => String -> Cmd expr env wrt st ()
cmdParse inp
    = do cenv <- ask
         cmd  <- _parse cenv inp
         cmdEval cmd

cmdEval :: (Show wrt) => Command expr -> Cmd expr env wrt st ()
cmdEval CNoop
    = cmdLoop

cmdEval (CErr msg)
    = do putErr msg
         ifInteractive cmdLoop cmdLoopExit	-- abort batch programs when syntax errors occur

cmdEval (CIncomplete part)
    = do cenv <- ask
         prompt <- _prompt2 cenv
         line   <- _read    cenv prompt
         maybe (cmdEval $ CErr $ "EOF on input when parsing: " ++ part)
               (\ l -> cmdParse $ part ++ "\n" ++ l)
               line

cmdEval (CExpr expr)
    = do cenv <- ask
         cst  <- get
         (res, st1, wrt) <- liftIO $ (_eval cenv) expr (_exprEnv cenv) (_exprState cst)
         put $ cst { _exprState = st1
                   , _cmdCnt    = _cmdCnt cst + 1
                   }
         putErr (show wrt)
         case res of
           Left msg
               -> cmdEval $ CErr msg
           Right r
               -> do putOut r
                     cmdLoop

cmdLoopExit :: Cmd expr env wrt st ()
cmdLoopExit
    = whenInteractive $
      ask >>= _bye >>= putErr

-- ------------------------------------------------------------

whenInteractive :: Cmd expr env wrt st () -> Cmd expr env wrt st ()
whenInteractive cmd
    = ifInteractive cmd $ return ()

ifInteractive :: Cmd expr env wrt st () -> Cmd expr env wrt st () -> Cmd expr env wrt st ()
ifInteractive thenPart elsePart
    = do b <- ask >>= _batch
         if (not b) then thenPart else elsePart

-- ------------------------------------------------------------

putErr :: (MonadIO m) => String -> m ()
putErr msg
    = liftIO $
      when (not . null $ msg) $
      do hPutStrLn stderr msg
         hFlush    stderr

putOut :: (MonadIO m) => String -> m ()
putOut msg
    = liftIO $
      do hPutStr stdout msg
         hFlush  stdout

-- ------------------------------------------------------------

type EvalExpr = Eval Env Log Store

data Env
    = Env Int

newtype Log
    = Log [String]
    deriving (Show, Monoid)

data Store
    = Store Int
      deriving (Show)

data Expr
    = Const Int
    | Var
    | Acc
    | Stor  Expr
    | Write Expr
    | Put   Expr
    | Plus  Expr Expr
    | Div   Expr Expr
      deriving (Show)

eval :: Expr -> EvalExpr Int
eval (Const i)
    = return i
eval Var
    = do (Env v) <- ask
         return v
eval Acc
    = do (Store v) <- get
         return v
eval (Stor e)
    = do v <- eval e
         put (Store v)
         return v
eval (Write e)
    = do v <- eval e
         tell (Log . return $ show v)
         return v
eval (Put e)
    = do v <- eval e
         liftIO $ putStrLn (show e ++ " -> " ++ show v)
         return v
eval (Plus e1 e2)
    = do v1 <- eval e1
         v2 <- eval e2
         return $ v1 + v2
eval (Div e1 e2)
    = do v1 <- eval e1
         v2 <- eval e2
         if v2 == 0
            then throwError $ "exception: " ++ show v1 ++ " div 0"
            else return $ v1 `div` v2

ee e = runEval (eval e) (Env 2) (Store 3) >>= print

newtype TclProg
    = TclProg [TclCmd]
      deriving (Show)

data TclCmd
    = TclCmd TclArg [TclArg]
      deriving (Show)

newtype TclArg
    = TclArg TclParts
      deriving (Show)

type TclParts
    = [TclPart]

data TclPart
    = TChars   String
    | TEscChar Char
    | TVar     String
    | TDQuote  TclParts
    | TBrace   TclParts
    | TBracket TclParts
      deriving (Show)

