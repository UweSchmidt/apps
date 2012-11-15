module  Language.Lua.VM.Core
where

import Control.Applicative ( (<$>) )

import Control.Monad.Error
-- import Control.Monad.Reader
import Control.Monad.State

import Data.Array.IArray
-- import Data.List                ( intercalate )

-- import Language.Common.Eval

import Language.Lua.VM.Instr
import Language.Lua.VM.Types
import Language.Lua.VM.Value

import System.IO                ( hPutStrLn
                                , stderr
                                )

-- ------------------------------------------------------------

luaError :: LuaError -> LuaAction res
luaError s
    = throwError s

-- ------------------------------------------------------------

execLoop :: LuaAction ()
execLoop
    = do ireg <- gets theIntrReg
         case ireg of
           Just _
               -> return ()     -- terminate loop, when interrupt is set
           Nothing
               -> ( ( getNextInstr >>= execInstr )
                    `catchError`
                    setIntrReg
                  ) >> execLoop

-- ------------------------------------------------------------

setIntrReg :: LuaError -> LuaAction ()
setIntrReg err
    = modify $
      \ s -> s { theIntrReg = Just err }

-- ------------------------------------------------------------

incrPC :: Int -> LuaAction ()
incrPC displ
    = modify $
      \ s -> s { thePC = CA $ theCA (thePC s) + displ }

getNextInstr :: LuaAction Instr
getNextInstr
    = gets thePC >>= getInstr

-- ------------------------------------------------------------

-- the ordered list of segments is searched for the
-- segment containing the instruction
-- an exec is raised, when the code address does not exist

getInstr :: CodeAddress -> LuaAction Instr
getInstr pc
    = do prog  <- gets theProg
         instr <- getI (theCA pc) prog
         logInstr (theCA pc) instr
         return instr
    where
      getI ic []
          = luaError $ "segmentation fault: illegal PC value " ++ show ic
      getI ic (((lb, ub), seg) : prog')
          | lb <= ic && ic <= ub
              = return $ seg ! ic
          | otherwise
              = getI ic prog'

-- ------------------------------------------------------------

-- load a module into the program store
-- for every code module, a segement implemeted as array is allocated
-- a new segment is allocated at the end of the segment list

loadCode :: LuaModule -> LuaAction CodeAddress
loadCode (MCode cs)
    = do prog <- gets theProg
         let (start, prog') = addInstr cs 0 prog
         modify $ \ s -> s { theProg = prog' }
         return $ CA start
    where
      addInstr is ic []
          = (ic, [(lb'ub, listArray lb'ub is)])
            where
              lb'ub = (ic, ic + length is - 1)
      addInstr is _ (seg : segs)
          = (ic, seg : segs')
            where
              (ic, segs') = addInstr is (ub + 1) segs
              ((_, ub), _) = seg

-- ------------------------------------------------------------

logInstr :: Int -> Instr -> LuaAction ()
logInstr pc instr
    = logMsg instrLog $ showMachineInstr pc instr

logValue :: String -> Value -> LuaAction ()
logValue msg val
    = logMsg evalLog $ dumpTOS msg val

setLoggingOn :: LuaAction ()
setLoggingOn
    = setLogging $ \ lg -> lg { logCmd = \ xs -> liftIO (hPutStrLn stderr xs) }

setLoggingOff :: LuaAction ()
setLoggingOff
    = setLogging $ \ lg -> lg { logCmd = const $ return () }

setLogging :: (LuaLogger -> LuaLogger) -> LuaAction ()
setLogging setf
    = modify $ \ s -> s { theLogger = setf $ theLogger s }

logMsg :: (LuaLogger -> Bool) -> String -> LuaAction ()
logMsg level msg
    = do logger <- gets theLogger
         when (level logger) $
                  (logCmd logger) msg


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

execInternal :: NativeFct -> LuaAction ()
execInternal nf
    = do vs     <- value2Tuple <$> popES        -- get the arguments
         rs     <- theNativeFct nf $ vs         -- call native function
         pushES rs                              -- store the result tuple

callInternal :: NativeFct -> LuaAction ()
callInternal nf
    = execInternal nf >> incrPC 1

jumpInternal :: NativeFct -> LuaAction ()
jumpInternal nf
    = execInternal nf >> leaveFct

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

pushES :: Value -> LuaAction ()
pushES v
    = do modify $ \ s -> s { theEvalStack = v : theEvalStack s }
         logValue "push" v
     

saveES :: Int -> Value -> LuaAction ()
saveES i v
    = do (vs0, es) <- gets (splitAt i . theEvalStack)
         if length vs0 == i
            then do logValue ("save " ++ show i) v
                    modify $ \ s -> s { theEvalStack = vs0 ++ v : es }
            else luaError . unwords $
                 ["not", show i, "values on evaluation stack"]

remES :: Int -> LuaAction ()
remES i
    = do (vs0, es) <- gets (splitAt i . theEvalStack)
         case es of
           (v : vs) -> do logValue ("rem " ++ show i) v
                          modify $ \ s -> s { theEvalStack = vs0 ++ vs }
           []        -> luaError $
                        "no value on evaluation stack at position " ++ show i

popES :: LuaAction Value
popES
    = do es <- gets theEvalStack
         case es of
           (v : vs) -> do modify $ \ s -> s { theEvalStack = vs }
                          logValue "pop" v
                          return v
           []       -> luaError "no value on evaluation stack"

getES :: Int -> LuaAction Value
getES i
    = do es <- gets (drop i . theEvalStack)
         case es of
           (v : _) -> do logValue ("get " ++ show i) v
                         return v
           []      -> luaError $
                      "no value on evaluation stack at position " ++ show i

takeES :: Int -> LuaAction Value
takeES i
    = do v <- getES i
         remES i
         return v

-- ------------------------------------------------------------

checkNum :: Value -> LuaAction Value
checkNum v
    = checkValue isNum "number" v
    where
      isNum (N _) = True
      isNum _     = False

checkNumOrString :: Value -> LuaAction Value
checkNumOrString v
    = checkValue isNumOrString "number or string" v
    where
      isNumOrString (N _) = True
      isNumOrString (S _) = True
      isNumOrString _     = False

checkNumOrNumString :: Value -> LuaAction Value
checkNumOrNumString v@(N _)
    = return v
checkNumOrNumString v
    = maybe (checkNum v) (return . N) $ value2number v

checkStringOrTable :: Value -> LuaAction Value
checkStringOrTable v
    = checkValue isST "string or table" v
    where
      isST (S _) = True
      isST (T _) = True
      isST _     = False

checkTuple :: Value -> LuaAction Value
checkTuple v
    = checkValue isTuple "tuple" v
    where
      isTuple (P _) = True
      isTuple _     = False

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

checkValue2 :: (Value -> LuaAction Value) ->
               (Value -> LuaAction Value) ->
               Value -> Value -> LuaAction (Value, Value)
checkValue2 c1 c2 v1 v2
    = c1 v1 >> c2 v2 >> return (v1, v2)

checkNumOrNumString2 :: Value -> Value -> LuaAction (Value, Value)
checkNumOrNumString2
    = checkValue2 checkNumOrNumString checkNumOrNumString

checkEqType :: String -> Value -> Value -> LuaAction ()
checkEqType op v1 v2
    | t1 == t2
        = return ()
    | otherwise
        = luaError $ unwords ["attempt to", op, t1, "with", t2]
    where
      t1 = luaType v1
      t2 = luaType v2

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

execInstr (Call)
    = do                -- old: v1   <- popES
         v1 <- takeES 1 -- new: top of stack: paramlist, top -1: closure
         fct  <- checkFctValue v1
         case fct of
           (C cls) -> callClosure  cls
           (F nf)  -> callInternal nf
           _       -> luaError $ "function value expected in function call, but got a " ++ show (luaType fct)

execInstr (TailCall)
    = do v1   <- takeES 1       -- old popES
         fct  <- checkFctValue v1
         case fct of
           (C cls) -> jumpClosure  cls
           (F nf)  -> jumpInternal nf
           _       -> luaError $ "function value expected in function tail call, but got a " ++ show (luaType fct)

execInstr (Leave)
    = leaveFct

execInstr (Intr msg)
    = luaError msg

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

execInstr1 (Copy i)
    = getES i >>= pushES

execInstr1 (Move i)
    = takeES i >>= pushES

execInstr1 (Save i)
    = popES >>= saveES i

-- ----------------------------------------
--
-- {- -- the new tuple instructions

execInstr1 (MkTup n)
    = execMkTup n
      where
        execMkTup i
            | i >= 2
                = execBinary (\ v -> return . consValues v)
                  >> execMkTup (i - 1)
            | i == 1
                = return ()
            | i == 0
                = pushES emptyTuple
            | otherwise
                = luaError $ "mktup with negative arg: " ++ show i

execInstr1 (UnTup n)
    = popES >>= execUnTup n
      where
        execUnTup i t
            | i >= 2
                = execUnTup (i - 1) v2
                  >> pushES v1
            | i == 1
                = pushES v1
            | i == 0
                = return ()
            | otherwise
                = luaError $ "untup with negative arg: " ++ show i
            where
              (v1, v2) = uncons t

execInstr1 (UnTup' n)
    = popES >>= execUnTup n
      where
        execUnTup i t
            | i == 1
                = pushES t
            | i >= 2
                = do execUnTup (i - 1) v2
                     pushES v1
            | otherwise
                = luaError $ "untup... with non positive arg: " ++ show i
            where
              (v1, v2) = uncons t

-- -}
-- ----------------------------------------
--
{- -- the old tuple instructions

-- LoadEmpty becomes MkTup 0
execInstr1 (LoadEmpty)
    = pushES emptyTuple

-- MkTuple becomes MkTup 2 
execInstr1 (MkTuple)
    = execBinary (\ v -> return . consValues v)

-- Pop becomes UnTup 0
execInstr1 (Pop)
    = popES >> return ()

-- UnTuple becomes UnTup... 2
execInstr1 (UnTuple)
    = do (v1, v2) <- uncons <$> popES
         pushES v2
         pushES v1

-- Take1 becomes UnTup 1
execInstr1 (Take1)
    = execUnary (return . singleValue)

-- -}
-- ----------------------------------------

-- table instructions

execInstr1 (NewTable)
    = do t <- newTable
         pushES (T t)

execInstr1 (LoadField)
    = execBinary loadField
      where
        loadField v1 v2
            = do (T t) <- checkTable v1         -- top of stack: index
                 readTable v2 t                 -- top -1:       table

execInstr1 (StoreField)
    = do val   <- popES                         -- top of stack: value
         ix    <- popES                         -- top -1:       index
         (T t) <- popES >>= checkTable          -- top -2:       table
         writeTable ix val t

execInstr1 (AppendField)
    = do v1    <- popES
         val   <- popES
         (T t) <- checkTable v1
         appendTable val t

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

execInstr1 instr
    = luaError $ "illegal or unimplemented instr: " ++ show instr

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
lookupOp2 Add  = numOp2 (+)
lookupOp2 Sub  = numOp2 (-)
lookupOp2 Mult = numOp2 (*)
lookupOp2 Exp  = numOp2 (**)
lookupOp2 Div  = numOp2 (/)
lookupOp2 Mod  = numOp2 $ \ x y -> x - fromIntegral (floor (x / y ) :: Int) * y
lookupOp2 EQU  = eqOp   (==)
lookupOp2 NEQ  = eqOp   (/=)
lookupOp2 GRT  = cmpOp  (>)
lookupOp2 GRE  = cmpOp  (>=)
lookupOp2 LSE  = cmpOp  (<=)
lookupOp2 LST  = cmpOp  (<)
lookupOp2 Conc = return $ \ x y -> checkNumOrString x
                                   >> checkNumOrString y
                                   >> (return . S) (value2str x ++ value2str y)

{- all binary ops are implemented
lookupOp2 op
    = luaError $ "unimplemented binary op: " ++ show op
-}

numOp2 :: (Double -> Double -> Double) -> LuaAction (Value -> Value -> LuaAction Value)
numOp2 op
    = return $
      \ v1 v2 -> do (N n1, N n2) <- checkNumOrNumString2 v1 v2
                    return $ N (n1 `op` n2)

eqOp :: (Value -> Value -> Bool) -> LuaAction (Value -> Value -> LuaAction Value)
eqOp op
    = return $
      \ x y -> return $ B (x `op` y)

cmpOp :: (Value -> Value -> Bool) -> LuaAction (Value -> Value -> LuaAction Value)
cmpOp op
    = return $
      \ x y -> checkEqType "compare" x y
               >> checkNumOrString x
               >> (return . B) (x `op` y)

-- ------------------------------------------------------------

lookupOp1 :: UOp -> LuaAction (Value -> LuaAction Value)
lookupOp1 Minus
    = return $
      \ v1 -> do (N n) <- checkNum v1
                 return $ N (0 - n)

lookupOp1 Not
    = return $
      \ v1 -> return $ B (isFalse v1)

lookupOp1 NumberOf
    = return $
      \ v1 -> do v' <- checkStringOrTable v1
                 case v' of
                   (S s) -> return $ int2Value $ length s
                   (T t) -> lengthTable t
                   _     -> luaError "illegal operand to # op"

{- all unary ops are implemented

lookupOp1 op
    = luaError $ "unimplemented unary op: " ++ show op
-}         
-- ------------------------------------------------------------
