module PPL.ControlUnit
    (execProg) where

import PPL.Instructions
import PPL.MachineArchitecture
import PPL.MicroCode
import PPL.OPCode

import PPL.ShowCode
import PPL.ShowMS

-- -------------------------------------------------------------------
-- exec program

execProg        :: MS -> IO ()
execProg
    = runProg startProg

runProg :: MST () -> MS -> IO ()
runProg (MST fct) initState
    = fct initState >> return ()

startProg       :: MST ()
startProg
    = do
      trc (\_ -> "start execution\n")
      continueProg

continueProg    :: MST ()
continueProg 
    = do
      trc showCurInstr
      succeed execInstr
      checkMStatus

execInstr       :: MST ()
execInstr
    = do
      instr' <- loadInstr
      incrProgCount
      intpInstr instr'

checkMStatus    :: MST ()
checkMStatus
    = do
      cont <- checkStateWith statusIsOk
      if cont
         then continueProg
         else do
              term <- checkStateWith statusIsTerminated
              if term
                 then termProg
                 else trc showMS

termProg                :: MST ()
termProg
    = trc (\_ -> "\nexecution terminated\n")

-- -------------------------------------------------------------------

intpInstr       :: Instr -> MST ()

intpInstr (LoadI i)     = pushMV (VInt i)
intpInstr (LoadF f)     = pushMV (VFloat f)
intpInstr (LoadS s)     = pushMV (VString s)
intpInstr  LoadU        = pushMV  VUndef
intpInstr  LoadEL       = pushMV (VList [])

intpInstr (Load addr)
    = do
      v0 <- readMV addr
      v1 <- checkNotUndef v0
      pushMV v1

intpInstr (Compute op)
    = case lookup op ops
      of
      Just (getArgs, eval)
          -> do
             vl  <- getArgs
             res <- eval vl
             pushMV res
      Nothing
          -> throw ("unimplemented op: " ++ showOpCode op)

intpInstr (Store addr)
    = do
      v <- popMV
      writeMV addr v

intpInstr  Pop
    = do
      _v <- popMV
      return ()

intpInstr  Dup
    = do
      v <- popMV
      pushMV v
      pushMV v

intpInstr (PushJ (Disp d))
    = do
      pc' <- getProgCount
      pushMV (VCodeAddr pc')
      setProgCount (pc' - 1 + d)

intpInstr  PopJ
    = do
      (VCodeAddr pc') <- popRA
      setProgCount pc'

intpInstr (Jump (Disp d))
    = do
      pc' <- getProgCount
      setProgCount (pc' - 1 + d)

intpInstr (Branch cond (Disp d))
    = do
      (VInt b)  <- popInt
      if (b /= 0) == cond
        then (do
              pc' <- getProgCount
              setProgCount (pc' - 1 + d)
             )
        else return ()

intpInstr (Entry len)
    = allocSF len

intpInstr Exit
    = freeSF

intpInstr (SysCall call)
    = case lookup call svcs
      of
      Just (getArgs, eval)
          -> do
             vl  <- getArgs
             res <- eval vl
             pushMV res
      Nothing
          -> throw ("unimplemented system call: " ++ call)

intpInstr instr'
    = throw ("unimplemented: " ++ show instr')

-- -------------------------------------------------------------------
