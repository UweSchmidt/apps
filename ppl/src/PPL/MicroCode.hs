module PPL.MicroCode where

import           Control.Monad

import           PPL.Error
import           PPL.Instructions
import           PPL.MachineArchitecture
import           PPL.ShowMS

import           System.IO

-- -------------------------------------------------------------------
--
-- machine state transitions

newtype MST a = MST { trans :: MS -> IO (MS, Maybe a) }

instance Functor MST where
    fmap = liftM

instance Applicative MST where
    pure  = return
    (<*>) = ap

instance Monad MST where
    return v
        = MST ( \s -> return (s, Just v))

    MST cmd >>= f
        = MST ( \s ->
                 cmd s >>= \ (s', r) ->
                 case r of
                 Just v  -> trans (f v) s'
                 Nothing -> return (s', Nothing)
               )

-- --------------------
--
-- lift IO comand to MST

io :: IO a -> MST a
io ioa =
    MST ( \s ->
           do
           a <- ioa
           return (s, Just a)
         )

-- --------------------
--
-- lift state modifying function to MST

changeStateWith :: (MS -> MS) -> MST ()
changeStateWith fct
    = MST ( \s ->
            return (fct s, Just ())
          )

-- --------------------
--
-- lift state modifying function to MST

readAndChangeStateWith  :: (MS -> (MS, a)) -> MST a
readAndChangeStateWith fct
    = MST ( \s ->
            let (s', v) = fct s
            in
            return (s', Just v)
          )

-- --------------------
--
-- lift state reading function to MST

readStateWith   :: (MS -> a) -> MST a
readStateWith fct
    = MST ( \s ->
            return (s, Just (fct s))
          )

checkStateWith  :: (MS -> a) -> MST a
checkStateWith  = readStateWith

-- --------------------
--
-- lift compute and IO functions

liftCompute             :: ([a] -> Error a) -> ([a] -> MST a)
liftCompute fct
    = \ vl -> MST ( \ s ->
                    return (case fct vl of
                            (OK v)      -> (s, Just v)
                            (Error err) -> (setExc err s, Nothing)
                           )
                  )

liftSvc                 :: ([a] -> IO (Error a)) -> ([a] -> MST a)
liftSvc fct
    = \ vl -> MST ( \ s ->
                    do
                    res <- fct vl
                    return (case res of
                            (OK v)      -> (s, Just v)
                            (Error err) -> (setExc err s, Nothing)
                           )
                  )

-- --------------------
--
-- state inspection IO command

trc     :: (MS -> String) -> MST ()
trc fct
    = MST ( \s ->
             do
             hPutStr stderr $ fct s
             return (s, Just ())
           )

-- --------------------
--
-- run a MST comand

run :: MS -> MST () -> IO()
run initState (MST cmd)
    = cmd initState >> return ()

-- --------------------
--
-- throw an exception: set status and abort further computation

throw           :: String -> MST a
throw err
    = MST ( \s ->
            return (setExc err s, Nothing)
          )

-- --------------------
--
-- catch all errors

succeed :: MST () -> MST ()
succeed (MST cmd)
    = MST ( \ s ->
            cmd s >>= \ (s', r) ->
            case r of
            Just _v -> return (s', r)
            Nothing -> return (s', Just ())
          )

-- -------------------------------------------------------------------

loadInstr       :: MST Instr
loadInstr
    = do
      pcIsOk <- checkStateWith legalPc
      if pcIsOk
         then readStateWith getInstr
         else do
              illegalPc <- readStateWith showIllegalPC
              throw illegalPc

incrProgCount   :: MST ()
incrProgCount   = changeStateWith incrPc

setProgCount    :: Int -> MST ()
setProgCount p  = changeStateWith (setPc p)

getProgCount    :: MST Int
getProgCount    = readStateWith getPc

allocSF         :: Int -> MST ()
allocSF size    = changeStateWith (allocFrame size)

freeSF          :: MST ()
freeSF
    = do
      empty <- checkStateWith nullFrames
      if not empty
         then changeStateWith freeFrame
         else throw showFrameStackUnderflow

pushMV          :: MV -> MST ()
pushMV v        = changeStateWith (pushValue v)

popMV           :: MST MV
popMV
    = do
      empty <- checkStateWith emptyStack
      if not empty
         then readAndChangeStateWith popValue
         else throw showStackUnderflow

readMV          :: Address -> MST MV
readMV addr
    = do
      addrOk <- checkStateWith (legalMemAddr addr)
      if addrOk
         then readStateWith (readMem addr)
         else do
              illegalAddr <- readStateWith (showIllegalRead addr)
              throw illegalAddr

writeMV         :: Address -> MV -> MST ()
writeMV addr v
    = do
      addrOk <- checkStateWith (legalMemAddr addr)
      if addrOk
         then changeStateWith (writeMem addr v)
         else do
              illegalAddr <- readStateWith (showIllegalWrite addr)
              throw illegalAddr

clearMStatus    :: MST ()
clearMStatus    = changeStateWith clearStatus

checkType       :: (MV -> Bool) -> String -> MV -> MST MV
checkType isOfType err v
    = if isOfType v
      then return v
      else throw (showIllegalOperand err v)

checkNotUndef, checkInt, checkFloat, checkString, checkList, checkPic,
 checkRA        :: MV -> MST MV

checkNotUndef   = checkType isNotUndef  "not an undefined value"
checkInt        = checkType isInt       "int"
checkFloat      = checkType isFloat     "float"
checkString     = checkType isString    "string"
checkList       = checkType isList      "list"
checkPic        = checkType isPic       "picture"
checkRA         = checkType isRA        "return address"

popVal          :: (MV -> MST a) -> MST a
popVal check    = do
                  v <- popMV
                  check v

popInt, popFloat, popString, popList, popPic,
 popRA          :: MST MV

popInt          = popVal checkInt
popFloat        = popVal checkFloat
popString       = popVal checkString
popList         = popVal checkList
popPic          = popVal checkPic
popRA           = popVal checkRA

-- -------------------------------------------------------------------

isNotUndef, isInt, isFloat, isString, isList, isPic,
 isRA   :: MV -> Bool

isNotUndef VUndef       = False
isNotUndef _            = True

isInt (VInt _)          = True
isInt _                 = False

isFloat (VFloat _)      = True
isFloat _               = False

isString (VString _)    = True
isString _              = False

isList (VList _)        = True
isList _                = False

isPic (VPic _)          = True
isPic _                 = False

isRA (VCodeAddr _)      = True
isRA _                  = False

-- -------------------------------------------------------------------
