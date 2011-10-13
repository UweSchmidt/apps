module  Language.Lua.VM.Core
where

import Control.Applicative ( (<$>) )

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.Array.IArray

import Language.Common.Eval

import Language.Lua.VM.Instr
import Language.Lua.VM.Types
import Language.Lua.VM.Value

-- ------------------------------------------------------------

runLua :: LuaAction res -> LuaProg -> LuaState -> IO (Either LuaError res, LuaState)
runLua act prog s0
    = runEval act emptyLuaEnv (loadProg prog s0)

luaError :: LuaError -> LuaAction res
luaError s
    = throwError s

loadProg :: LuaProg -> LuaState -> LuaState
loadProg (MCode is) s0
    = s0 { theProg = listArray (0, length is - 1) is }

-- ------------------------------------------------------------

incrPC :: Int -> LuaAction ()
incrPC displ
    = modify $
      \ s -> s { thePC = CA $ theCA (thePC s) + displ }

getInstr :: LuaAction Instr
getInstr
    = do pc    <- gets thePC
         instr <- gets ((! theCA pc) . theProg)
         traceInstr (theCA pc) instr
         return instr

-- ------------------------------------------------------------

traceInstr :: Int -> Instr -> LuaAction ()
traceInstr pc instr
    = do logger <- gets theLogger
         logger (showMachineInstr pc instr)

-- ------------------------------------------------------------
--
-- environment primitives

openEnv :: LuaAction ()
openEnv
    = do t <- newTable
         modify $ \ s -> s { theCurrEnv = Env . (t :) . theEnv $ theCurrEnv s }
         

closeEnv :: LuaAction ()
closeEnv
    = do ts <- gets (theEnv . theCurrEnv)
         case ts of
           (_ : ts') -> modify $  \ s -> s { theCurrEnv = Env ts' }
           []        -> luaError "no local env to close"

-- ------------------------------------------------------------

callClosure :: Closure -> LuaAction ()
callClosure cls
    = modify $ call
      where
        call s
            = s { theCurrEnv   = theClosureEnv cls
                , thePC        = theCodeAddr cls
                , theCallStack = retCls : theCallStack s
                }
            where
              retCls
                  = CL { theClosureEnv = theCurrEnv s
                       , theCodeAddr   = CA ((theCA . thePC) s + 1)
                       }

jumpClosure :: Closure -> LuaAction ()
jumpClosure cls
    = modify $ jmp
      where
        jmp s
            = s { theCurrEnv   = theClosureEnv cls
                , thePC        = theCodeAddr cls
                }

callInternal :: NativeFct -> LuaAction ()
callInternal nf
    = do (L vs) <- popES >>= checkList		-- get the arguments
         rs     <- theNativeFct nf $ vs         -- call native function
         pushES (L rs)                          -- store the results

jumpInternal :: NativeFct -> LuaAction ()
jumpInternal nf
    = callInternal nf >> leaveFct

leaveFct :: LuaAction ()
leaveFct
    = do cs <- gets theCallStack
         case cs of
           []          -> luaError "call stack underflow in leave instr"
           (cls : cs') -> modify $ leave cls cs'
    where
      leave cls cs s
          = s { theCurrEnv   = theClosureEnv cls
              , thePC        = theCodeAddr   cls
              , theCallStack = cs
              }

-- ------------------------------------------------------------
--
-- evaluation stack primitives

popES :: LuaAction Value
popES
    = do es <- gets theEvalStack
         case es of
           (v : vs) -> do modify $ \ s -> s { theEvalStack = vs }
                          return v
           []       -> luaError "no value on evaluation stack"

pushES :: Value -> LuaAction ()
pushES v
    = modify $
      \ s -> s { theEvalStack = v : theEvalStack s }

getES :: Int -> LuaAction Value
getES i
    = do es <- gets (drop i . theEvalStack)
         case es of
           (v : _) -> return v
           []      -> luaError $
                      "no value on evaluation stack at position " ++ show i

remES :: Int -> LuaAction ()
remES i
    = do (vs0, es) <- gets (splitAt i . theEvalStack)
         case es of
           (_v : vs) -> modify $ \ s -> s { theEvalStack = vs0 ++ vs }
           []        -> luaError $
                        "no value on evaluation stack at position " ++ show i

-- ------------------------------------------------------------

checkList :: Value -> LuaAction Value
checkList v
    = checkValue isList "list" v
    where
      isList (L _) = True
      isList _     = False

checkTable :: Value -> LuaAction Value
checkTable v
    = checkValue isTab "table" v
    where
      isTab (T _) = True
      isTab _     = False

checkFctValue :: Value -> LuaAction Value
checkFctValue v
    = checkValue isFct "function" v
    where
      isFct (C _) = True
      isFct (F _) = True
      isFct _     = False

checkValue :: (Value -> Bool) -> String -> Value -> LuaAction Value
checkValue p err v
    | p v
        = return v
    | otherwise
        = luaError $ err ++ " expected but got a value of type " ++ show (luaType v)


-- ------------------------------------------------------------
--
-- first check for control instructions
-- these manipulate the pc explicitly

execInstr :: Instr -> LuaAction ()

execInstr (Jump (D displ))
    = incrPC displ

execInstr (Branch c (D displ))
    = do v <- popES
         incrPC $ if isTrue v == c
                  then displ
                  else 1

execInstr instr@(Call)
    = luaError $ "unimplemented instr: " ++ show instr

execInstr instr@(TailCall)
    = luaError $ "unimplemented instr: " ++ show instr

execInstr instr@(Leave)
    = luaError $ "unimplemented instr: " ++ show instr

execInstr instr@(Exit _rc)
    = luaError $ "unimplemented instr: " ++ show instr

execInstr instr
    = execInstr1 instr >> incrPC 1

-- ------------------------------------------------------------
--
-- instructions where the PC is incremented
-- first the instructions to move values from and to
-- the evaluation stack

execInstr1 :: Instr -> LuaAction ()
execInstr1 (BinOp op)
    = lookupOp2 op >>= execBinary

execInstr1 (UnOp op)
    = lookupOp1 op >>= execUnary

execInstr1 (LoadNum d)
    = pushES $ N d

execInstr1 (LoadStr s)
    = pushES $ S s

execInstr1 (LoadBool b)
    = pushES $ B b

execInstr1 (LoadNil)
    = pushES nil

execInstr1 (LoadEmpty)
    = pushES emptyList

execInstr1 (Pop)
    = popES >> return ()

execInstr1 (Copy i)
    = do v <- getES i
         pushES v

execInstr1 (Move i)
    = do v <- getES i
         remES i
         pushES v

execInstr1 (MkTuple)
    = execBinary (\ v -> return . consValues v)

execInstr1 (UnTuple)
    = do (v1, v2) <- uncons <$> popES
         pushES v2
         pushES v1

execInstr1 (Take1)
    = execUnary (return . list2Value)

-- table instructions

execInstr1 (NewTable)
    = do t <- newTable
         pushES (T t)

execInstr1 (LoadField)
    = execBinary loadField
      where
        loadField v1 v2
            = do (T t) <- checkTable v1
                 readTable v2 t 

execInstr1 (StoreField)
    = do v1    <- popES
         ix    <- popES
         val   <- popES
         (T t) <- checkTable v1
         writeTable ix val t

execInstr1 (Append)
    = execBinary append
      where
        append v1 v2
            = do (T t) <- checkTable v1
                 appendTable v2 t
                 return v1

-- env instructions

execInstr1 (LoadVar n)
    = do v <- gets theCurrEnv >>= readVariable (S n)
         pushES v

execInstr1 (StoreVar n)
    = do v <- popES
         gets theCurrEnv >>= writeVariable (S n) v

execInstr1 (NewLocal n)
    = gets theCurrEnv >>= newLocalVariable (S n)

execInstr1 (NewEnv)
    = openEnv

execInstr1 (DelEnv)
    = closeEnv

-- subroutine instructions

execInstr1 (Closure (D displ))
    = do (CA ic) <- gets thePC
         env     <- gets theCurrEnv
         pushES $ C $ CL { theClosureEnv = env
                         , theCodeAddr   = CA $ ic + displ
                         }

execInstr1 (Call)
    = do v1   <- popES
         fct  <- checkFctValue v1
         case fct of
           (C cls) -> callClosure  cls
           (F nf)  -> callInternal nf
           _       -> luaError $ "function value expected in function call, but got a " ++ show (luaType fct)

execInstr1 (TailCall)
    = do v1   <- popES
         fct  <- checkFctValue v1
         case fct of
           (C cls) -> jumpClosure  cls
           (F nf)  -> jumpInternal nf
           _       -> luaError $ "function value expected in function tail call, but got a " ++ show (luaType fct)

execInstr1 (Leave)
    = leaveFct

-- the rest

execInstr1 instr
    = luaError $ "unimplemented instr: " ++ show instr

-- ------------------------------------------------------------

execBinary :: (Value -> Value -> LuaAction Value) -> LuaAction ()
execBinary f
    = do v2 <- popES
         v1 <- popES
         r  <- f v1 v2
         pushES r

execUnary :: (Value -> LuaAction Value) -> LuaAction ()
execUnary f
    = do v1 <- popES
         r  <- f v1
         pushES r

-- ------------------------------------------------------------

lookupOp2 :: BOp -> LuaAction (Value -> Value -> LuaAction Value)
lookupOp2 op
    = luaError $ "unimplemented binary op: " ++ show op
         
lookupOp1 :: UOp -> LuaAction (Value -> LuaAction Value)
lookupOp1 op
    = luaError $ "unimplemented unary op: " ++ show op
         
-- ------------------------------------------------------------
