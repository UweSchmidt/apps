module Language.Tcl.Core
where

import           Control.Arrow          ( second
                                        , (***)
                                        )
import           Control.Applicative    ( (<$>) )
import           Control.Monad.Error
import           Control.Monad.RWS

import           Data.List              ( intercalate
                                        , isPrefixOf
                                        )
import           Data.Map    		( Map )
import qualified Data.Map      		as M
import           Data.Maybe             ( fromMaybe )
import           Data.Set    		( Set )
import qualified Data.Set      		as S

import           Language.Common.Eval

import           Language.Tcl.Value
import           Language.Tcl.AbstractSyntax
import qualified Language.Tcl.Parser    as P

import           System.IO

-- ------------------------------------------------------------

data TclApp e s
    = TclApp
      { _theTclEnv   :: TclEnv e
      , _theTclState :: TclState e s
      }

data TclEnv e
    = TclEnv
      { _tclLogger :: TclLogger
      , _appEnv :: e
      }

instance (Show e) => Show (TclEnv e) where
    show (TclEnv _lg ae)
        = "TclEnv { _appEnv = " ++ show ae ++ " }"

type TclLogger
    = String -> IO ()

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
        = unlines
          [ "TclState"
          , "{ _tglobalVars = " ++ show v
          -- ++ (show . map (second selS) . M.toList $ v)
          -- ++ ", _tcmds = "
          -- ++ (show . M.keys $ c)
          , ", _tChans = " ++ (show . M.keys $ ch)
          , ", _appState = " ++ show as
          , "}"
          ]

type TclWrt		-- not really used
    = String

data TclVars
    = TVS
      { _tsimple :: Map String Value
      , _tarray  :: Map String (Map String Value)
      }
      -- deriving (Show)

instance Show TclVars where
    show (TVS s a)
        = showMap s ++ ", " ++
          (show . map (second $ show . M.toList) . M.toList) a
        where
          showMap = intercalate ", " . map (show . second show) . M.toList

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
      { _fparams  :: Value	                  -- the source string of the list of formal param names
                                                  -- and default values
      , _fbody    :: Value                        -- the source string of the body
      , _cbody    :: TclEval e s Value	          -- compiled body
      , _cpassing :: Values -> TclEval e s Value  -- compiled param passing
      }

type TclChannels	-- open channels
    = Map String Handle

data TclError
    = TclError
      { _tclErrorLevel :: Int
      , _tclErrorMsg   :: Value
      }
      deriving (Show)

type TclEval e s
    = Eval TclError (TclEnv e) (TclState e s)

type TclLib e s
    = (TclEval e s (), [(String, TclCommand e s)])

type TclVarName
    = (String, Maybe String)

-- ------------------------------------------------------------

emptyTclVars :: TclVars
emptyTclVars = TVS
               { _tsimple = M.empty
               , _tarray  = M.empty
               }

emptyTclCommands :: TclCommands e s
emptyTclCommands = M.empty

emptyTclProcs :: TclProcs e s
emptyTclProcs = M.empty

emptyTclChannels :: TclChannels
emptyTclChannels = M.empty

-- ------------------------------------------------------------

instance Error TclError where
    noMsg  = tclErrorExc "unknonw Tcl error"
    strMsg = tclErrorExc 

tclErrorExc :: String -> TclError
tclErrorExc
    = TclError 1 . mkS

tclReturnExc :: Value -> TclError
tclReturnExc
    = TclError 2

tclBreakExc :: TclError
tclBreakExc
    = TclError 3 mempty

tclContinueExc :: TclError
tclContinueExc
    = TclError 4 mempty

tclOtherExc :: Int -> Value -> TclError
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
    = tclTryCatch (\ (TclError _lev val) -> return val)

tclChangeErr :: Value -> (Int -> Bool) -> TclEval e s Value -> TclEval e s Value
tclChangeErr val
    = tclTryCatch (\ (TclError lev _val) -> throwError (tclOtherExc lev val))

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
evalTcl cmd@(cn : args)
    = do logCommand cmd
         c <- lookupProcOrCmd (selS cn)
         c args
evalTcl []
    = tclThrowError "empty command"

evalTclArg	:: TclArg -> TclEval e s Value
evalTclArg (TclArg xs)
    = mapM evalTclSubst xs >>= return . mconcat

evalTclSubst	:: TclSubst -> TclEval e s Value
evalTclSubst (TLit s)
    = return $ mkS s

evalTclSubst (TVar n Nothing)
    = lookupVar (n, Nothing)

evalTclSubst (TVar n (Just ix))
    = do i <- selS <$> evalTclArg (TclArg ix)
         lookupVar (n, Just i)

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

setCmd :: String -> TclCommand e s -> TclEval e s ()
setCmd n cmd
    = modify (\ s ->  s { _tcmds = M.insert n cmd $ _tcmds s })

lookupProc :: String -> TclEval e s (TclProc e s)
lookupProc n
    = get
      >>=
      maybe (tclThrowError $ "invalid proc name " ++ show n) return
                . M.lookup n
                . _tprocs

setProc :: String -> TclProc e s -> TclEval e s ()
setProc n prc
    = modify (\ s ->  s { _tprocs = M.insert n prc $ _tprocs s })

lookupProcOrCmd :: String -> TclEval e s (TclCommand e s)
lookupProcOrCmd n
    = ( buildProcCall <$> lookupProc n )
      `mplus`
      lookupCmd n
    where
      buildProcCall tp
          = \ vs ->
            ( pushStackFrame n
              >> (_cpassing tp) vs
              >> (tclCatchReturnExc $_cbody tp)
            )
            `finallyError`
            popStackFrame       


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
                     = TPF { _tlocals = emptyTclVars
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

lookupVN :: TclVarName -> TclVars -> Maybe Value
lookupVN (n, ix) vm
    = case ix of
        Nothing -> M.lookup n $ _tsimple vm
        Just i  -> M.lookup n (_tarray vm) >>= M.lookup i

lookupAVN :: TclVarName -> TclVars -> Maybe (Map String Value)
lookupAVN (n, ix) vm
    = case ix of
        Nothing -> M.lookup n $ _tarray vm
        Just _  -> Nothing

insertVN :: TclVarName -> Value -> TclVars -> TclVars
insertVN (n, ix) v vm@TVS{ _tsimple = s, _tarray = a}
    = case ix of
        Nothing -> vm { _tsimple = M.insert n v s }
        Just i  -> let mx = M.insert i v . fromMaybe M.empty $ M.lookup n a
                   in
                     vm { _tarray  = M.insert n mx a }

deleteVN :: TclVarName -> TclVars -> TclVars
deleteVN (n, ix) vm@TVS{ _tsimple = s, _tarray = a}
    = case ix of
        Nothing -> vm { _tsimple = M.delete n s
                      , _tarray  = M.delete n a
                      }
        Just i  -> case M.lookup n a of
                     Nothing -> vm
                     Just mx -> let mx' = M.delete i mx
                                in
                                  vm { _tarray = M.insert n mx' a }

createAVN :: TclVarName -> TclVars -> TclVars
createAVN (n, ix) vm@TVS{ _tarray = a}
    = case ix of
        Nothing -> vm { _tarray  = if not (n `M.member` a)
                                   then M.insert n M.empty a
                                   else a
                      }
        Just _  -> vm

isVN :: TclVarName -> TclVars -> Bool
isVN (n, ix) TVS{ _tsimple = s, _tarray = a}
    = case ix of
        Nothing -> n `M.member` s || n `M.member` a
        Just i  -> maybe False (i `M.member`) $ M.lookup n a

isSVN :: TclVarName -> TclVars -> Bool
isSVN (n, ix) TVS{ _tsimple = s}
    = case ix of
        Nothing -> n `M.member` s
        Just _i -> False

isAVN :: TclVarName -> TclVars -> Bool
isAVN (n, ix) TVS{ _tarray = a}
    = case ix of
        Nothing -> n `M.member` a
        Just _i -> False

namesVN :: TclVars -> [String]
namesVN vm
    = namesSVN vm ++ namesAVN vm

namesSVN :: TclVars -> [String]
namesSVN
    = M.keys . _tsimple

namesAVN :: TclVars -> [String]
namesAVN
    = M.keys . _tarray

selVN :: Value -> TclVarName
selVN = splitVarName . selS

-- ------------------------------------------------------------

lookupLocalVar' :: (TclVarName -> TclVars -> Maybe a) -> TclVarName -> TclEval e s a
lookupLocalVar' lookupVN' n
    = do stack <- _tstack <$> get
         case stack of
           []  -> notFound
           (frame : _)
               -> if fst n `S.member` _tglobals frame
                  then notFound
                  else case lookupVN' n $ _tlocals frame of
                         Nothing -> notFound
                         Just v  -> return v
    where
      notFound = tclThrowError $ "can't read " ++ show (joinVarName n) ++ ": no such local variable"

lookupGlobalVar' :: (TclVarName -> TclVars -> Maybe a) -> TclVarName -> TclEval e s a
lookupGlobalVar' lookupVN' n
    = do st <- get
         case lookupVN' n $ _tglobalVars st of
           Nothing
               -> tclThrowError $ "can't read " ++ show (joinVarName n) ++ ": no such global variable"
           Just c
               -> return c

lookupVar'	:: (TclVarName -> TclVars -> Maybe a) -> TclVarName -> TclEval e s a
lookupVar' lookupVN' n
    = lookupLocalVar' lookupVN' n
      `mplus` lookupGlobalVar' lookupVN' n
      `mplus` (tclThrowError $ "can't read " ++ show (joinVarName n) ++ ": no such variable")

lookupVar	:: TclVarName -> TclEval e s Value
lookupVar
    = lookupVar' lookupVN

lookupArrayVar	:: TclVarName -> TclEval e s (Map String Value)
lookupArrayVar
    = lookupVar' lookupAVN

-- ------------------------------------------------------------

splitVarName :: String -> TclVarName
splitVarName n
    | null ix
      ||
      last ix /= ')'
        = (n, Nothing)
    | otherwise
        = (vn, Just . init . tail $ ix)
    where
      (vn, ix) = break (== '(') n

joinVarName :: TclVarName -> String
joinVarName (vn, ix)
    = vn ++ maybe "" (("(" ++) . (++ ")")) ix

-- ------------------------------------------------------------

setLocalVar :: TclVarName -> Value -> TclEval e s Value
setLocalVar n v
    = modifyLocalVar "can't write local variable" n (flip insertVN v) >> return v

setGlobalVar :: TclVarName -> Value -> TclEval e s Value
setGlobalVar n v
    = modifyGlobalVar n (flip insertVN v) >> return v

setVar		:: TclVarName -> Value -> TclEval e s Value
setVar n v
    = setLocalVar n v `mplus` setGlobalVar n v

-- ------------------------------------------------------------

modifyLocalVar :: String -> TclVarName -> (TclVarName -> TclVars -> TclVars) -> TclEval e s ()
modifyLocalVar msg n cf
    = do s <- get
         let stack = _tstack s
         case stack of
           [] -> notFound
           (frame : rest)
               -> if fst n `S.member` _tglobals frame
                  then notFound
                  else do let vars'  = cf n $ _tlocals frame
                          let frame' = frame { _tlocals = vars' }
                          let stack' = frame' : rest
                          put $ s { _tstack = stack' }
         return ()
    where
      notFound = tclThrowError $ msg ++ " " ++ show (joinVarName n) ++ ": no such variable"

modifyGlobalVar :: TclVarName -> (TclVarName -> TclVars -> TclVars) -> TclEval e s ()
modifyGlobalVar n cf
    = modify $ \ s -> s { _tglobalVars = cf n (_tglobalVars s) }

-- ------------------------------------------------------------

unsetLocalVar	:: TclVarName -> TclEval e s Value
unsetLocalVar n
    = modifyLocalVar "can't unset local variable" n deleteVN >> return mempty

unsetGlobalVar :: TclVarName -> TclEval e s Value
unsetGlobalVar n
    = do modifyGlobalVar n deleteVN
         return mempty

unsetVar	:: TclVarName -> TclEval e s Value
unsetVar n
    = unsetLocalVar n `mplus` unsetGlobalVar n

-- ------------------------------------------------------------

createLocalArray :: TclVarName -> TclEval e s ()
createLocalArray n
    = modifyLocalVar "can't create local array" n createAVN

createGlobalArray :: TclVarName -> TclEval e s ()
createGlobalArray n
    = modifyGlobalVar n createAVN

createArray	:: TclVarName -> TclEval e s ()
createArray n
    = createLocalArray n `mplus` createGlobalArray n

-- ------------------------------------------------------------

varName :: (TclVarName -> TclVars -> Bool) -> TclVarName -> TclEval e s Bool
varName isVN' n
    = do s <- get
         let vars  = _tglobalVars s
         let stack = _tstack s
         return $
           isVN' n vars
           ||
           ( (not . null $ stack)
             &&
             isVN' n (_tlocals . head $ stack)
           )

varNames :: TclEval e s [String]
varNames
    = do globals <- globalVarNames
         locals  <- localVarNames
         return $ locals ++ globals

globalVarNames :: TclEval e s [String]
globalVarNames
    = namesVN . _tglobalVars <$> get

localVarNames :: TclEval e s [String]
localVarNames
    = do stack <- gets _tstack
         return $
           case stack of
             [] -> []
             _  -> namesVN . _tlocals . head $ stack

-- ------------------------------------------------------------

lookupChannel	:: String -> TclState e s -> TclEval e s Handle
lookupChannel n s
    = case M.lookup n $ _tchans s of
        Nothing
            -> tclThrowError $ "can't find channel named " ++ show n
        Just c
            -> return c

setChannel :: String -> Handle -> TclEval e s ()
setChannel n h
    = modify $
      \ s -> s { _tchans = M.insert n h $ _tchans s }

-- ------------------------------------------------------------

loadTclLib :: TclLib e s -> TclEval e s ()
loadTclLib (initLib, cmdList)
    = ( sequence_ $ map (uncurry setCmd) cmdList )
      >>
      initLib

-- ------------------------------------------------------------

nsp :: String
nsp = "tcl_"

-- ------------------------------------------------------------

nameTraceLevel :: String
nameTraceLevel = nsp ++ "traceLevel"

traceCommands :: TclEval e s Bool
traceCommands
    = (>= 2) <$> getTraceLevel

traceComments :: TclEval e s Bool
traceComments
    = (>= 1) <$> getTraceLevel

getTraceLevel :: TclEval e s Integer
getTraceLevel
    = do st <- get
         return
           . fromMaybe 0
           . selI
           . fromMaybe value_0
           . lookupVN (nameTraceLevel, Nothing)
           $ _tglobalVars st

logCommand :: Values -> TclEval e s ()
logCommand cmd
    = do lg <- asks _tclLogger
         lv <- getTraceLevel
         when (lv == 3)			-- level == 3: all commands are traced
                  $ liftIOE $ lg cmdStr
         when ( cmdName == "#"          -- level `elem` [1,2]: comments (command name is "#") are traced
                &&                      -- level == 1: only if comments have the form "#@..."
                ( lv == 2               -- level == 2: all commands
                  || ( lv == 1
                       &&
                       "@" `isPrefixOf` arg1
                     )
                )
              ) $ liftIOE $ lg $ concatMap selS cmd
    where
      cmdName = concatMap selS . take 1          $ cmd
      arg1    = concatMap selS . take 1 . drop 1 $ cmd
      cmdStr  = unlines
                . uncurry (++)
                . (map ("+ " ++) *** map ("++" ++))
                . splitAt 1
                . lines
                . selS
                . mkL
                $ cmd

-- ------------------------------------------------------------
{-
tr :: (MonadIO m, Show a) => a -> m ()
tr v = liftIO $ print v
-}
-- ------------------------------------------------------------

