module PPL.MachineArchitecture where

import PPL.Instructions
import PPL.Picture

import Data.Array

type MProg      = Array Int Instr       -- the machine programm

-- machine values are tagged
-- there is an undefined value for initializing variable
-- if this value is read from the data memory, an error is issued
--

data MV
    = VUndef
    | VInt      Int
    | VFloat    Double
    | VString   String
    | VPic      Picture
    | VList     [MV]
    | VCodeAddr Int

data MStatus
    = Ok                -- continue
    | Exc       String  -- exception in instr exec

-- the memory for program variables

type Mem        = [MV]

-- the evaluation stack

type Stack      = [MV]

-- the complete machine state

data MS         = MS { instr    :: ! MProg
                     , pc       :: ! Int
                     , mem      :: ! Mem
                     , stack    :: ! Stack
                     , frames   :: ! [Mem]
                     , status   :: ! MStatus
                     }

-- -------------------------------------------------------------------

-- machine state access functions

-- -------------------------------------------------------------------

-- pc access

getPc           :: MS -> Int
getPc ms        = pc ms

setPc           :: Int -> MS -> MS
setPc pc1 ms    = ms { pc = pc1 }

incrPc          :: MS -> MS
incrPc ms       = ms { pc = (getPc ms + 1) }

legalPc         :: MS -> Bool
legalPc ms
    = pc1 >= lb && pc1 <= ub
      where
      (lb, ub) = bounds (instr ms)
      pc1 = pc ms

getInstr        :: MS -> Instr
getInstr ms
    = instr ms ! pc ms

-- -------------------------------------------------------------------

-- memory access

-- global memory

legalMemAddr    :: Address -> MS -> Bool
legalMemAddr (AbsA addr) ms
    = addr >= 0 && addr < ub
      where
      ub = length (mem ms)

legalMemAddr (LocA addr) ms
    = not (null stackframes)
      && addr >= 0
      && addr < ub
      where
      ub = length (head stackframes)
      stackframes = frames ms

readMem         :: Address -> MS -> MV
                -- pre: legalMemAddr
readMem (AbsA addr) ms
    = (mem ms) !! addr

readMem (LocA addr) ms
    = head (frames ms) !! addr


writeMem        :: Address -> MV -> MS -> MS
                -- pre: legalMemAddr
writeMem (AbsA addr) val ms
    = ms { mem = substVal addr val (mem ms) }

writeMem (LocA addr) val ms
    = ms { frames = substVal addr val top : rest }
      where
      (top : rest) = frames ms

substVal        :: Int -> MV -> Mem -> Mem
substVal ix val l
    = left ++ (val : tail right)
      where
      (left, right) = splitAt ix l

allocFrame      :: Int -> MS -> MS
allocFrame size ms
    = ms { frames = newframe : frames ms }
      where
      newframe = replicate size VUndef

freeFrame       :: MS -> MS
                -- pre: not empty stack of frames
freeFrame ms
    = ms { frames = tail (frames ms) }

nullFrames      :: MS -> Bool
nullFrames ms   = null (frames ms)

-- -------------------------------------------------------------------

-- stack access

pushValue :: MV -> MS -> MS
pushValue val ms
    = ms { stack = (val : stack ms) }

popValue        :: MS -> (MS, MV)
popValue ms
    = ( ms { stack = st1 }, val )
      where
      (val : st1) = stack ms
      -- pre: not emptyStack

emptyStack      :: MS -> Bool
emptyStack ms
    = null (stack ms)


-- -------------------------------------------------------------------
--
-- status register

programTerminated       :: String
programTerminated       = "program terminated"

programAborted          :: String
programAborted          = "program aborted"

statusIsTerminated      :: MS -> Bool
statusIsTerminated ms
    = case status ms
      of
      Exc e     -> e == programTerminated
      _         -> False

statusIsExc     :: MS -> Bool
statusIsExc ms
    = case status ms
      of
      Exc _e    -> not (statusIsTerminated ms)
      _         -> False

statusIsOk              :: MS -> Bool
statusIsOk ms
    = case status ms
      of
      Ok        -> True
      _         -> False

getExc  :: MS -> String
getExc ms
    = let
      Exc exc = status ms
      in
      exc

setExc          :: String -> MS -> MS
setExc e ms     = ms { status = Exc e }

clearStatus     :: MS -> MS
clearStatus ms  = ms { status = Ok }

-- -------------------------------------------------------------------
