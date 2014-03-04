module PPL.ShowMS where

import PPL.Instructions
import PPL.MachineArchitecture
import PPL.Picture
import PPL.ShowCode

import Data.Array

-- -------------------------------------------------------------------

-- dump machine state

showMS          :: MS -> String
showMS ms
    =  "\nprogram dump\n"
      ++ "============\n\n"
      ++ "status:\n"
      ++ showStatus (status ms)
      ++ "global data memory:\n"
      ++ showMem (mem ms)
      ++ "\nsubroutine stack frames:\n"
      ++ showFrames (frames ms)
      ++ "\nevaluation stack:\n"
      ++ showStack (stack ms)
      ++ "\nprogram counter:\n"
      ++ showPC (pc ms)
      ++ "\n"

showMSI         :: MS -> String
showMSI ms
    = showMS ms
      ++ "\ninstructions\n"
      ++ showMInstr (instr ms)

-- dump main memory

showMem         :: Mem -> String
showMem
    = concat . map showCell . zip [0..]

showCell        :: (Int, MV) -> String
showCell (i,v)
    = showTag (show i) ++ showMV v ++ "\n"

showFrames      :: [Mem] -> String
showFrames []
    = "\t\t<empty>\n"

showFrames fs
    = concat . map showFrame . zip [0..] $ fs

showFrame       :: (Int, Mem) -> String
showFrame (i,f)
    = "frame "
      ++ show i
      ++ ":\n"
      ++ showMem f

-- dump program counter

showPC          :: Int -> String
showPC pc1
    = showTag "pc" ++ show pc1 ++ "\n"

-- dump evaluation stack

showStack       :: Stack -> String
showStack []
    = "\t\t<empty>\n"

showStack s
    = concat . map showCell . zip [0..] $ s

-- dump program segment

showMInstr      :: MProg -> String
showMInstr
    = showCode1 . elems

-- dump machine values

showMV          :: MV -> String

showMV VUndef           = "undef"
showMV (VInt i)         = show i
showMV (VFloat f)       = show f
showMV (VString s)      = show s
showMV (VPic p)         = "<" ++ show (widthMx p) ++ "x" ++ show (heightMx p) ++ ">"
showMV (VList [])       = "[]"
showMV (VList l)        = "[" ++ (foldr1 (\x y -> x ++ ", " ++ y) (map showMV l)) ++ "]"
showMV (VCodeAddr a)    = "instr[" ++ show a ++ "]"

showTag         :: String -> String
showTag s
    = drop ls s'
    where
    s' = "         " ++ s ++ ":\t"
    ls = length s' - 9

showStatus      :: MStatus -> String
showStatus Ok
    = "\tok\n\n"

showStatus (Exc err)
    = "\texception: "
      ++ err
      ++ "\n\n"

-- -------------------------------------------------------------------

-- error messages
-- called when machine runs in an exceptional state

showCurInstr    :: MS -> String
showCurInstr ms
    | legalPc ms = showInstrCnt (pc ms) (instr ms ! pc ms)
    | otherwise  = "\n"

showIllegalPC   :: MS -> String
showIllegalPC ms
    = "pc out ouf code segment: pc = "
      ++ show (pc ms)
      ++ " ( "
      ++ show lb
      ++ " <= pc <= "
      ++ show ub
      ++ " ) "
    where
    (lb, ub) = bounds (instr ms)


showIllegalRead,
 showIllegalWrite       :: Address -> MS -> String

showIllegalRead         = showIllegalAddr "read"
showIllegalWrite        = showIllegalAddr "write"

showIllegalAddr         :: String -> Address -> MS -> String
showIllegalAddr err addr@(AbsA _) ms
    = err 
      ++ " absolut"
      ++ showIllegalAddr1 addr 0 ub
    where
    ub = length (mem ms) - 1
    

showIllegalAddr err addr@(LocA _) ms
    = err
      ++ " local"
      ++ showIllegalAddr1 addr 0 ub
    where
    ub    = length frame - 1
    frame = head (frames ms)

showIllegalAddr1        :: Address -> Int -> Int -> String
showIllegalAddr1 addr lb ub
    = " address out of data segment: addr = "
      ++ showAddr addr
      ++ " ( "
      ++ show lb
      ++ " <= addr <= "
      ++ show ub
      ++ " ) "


showStackUnderflow      :: String
showStackUnderflow
    = "evaluation stack underflow"

showFrameStackUnderflow :: String
showFrameStackUnderflow
    = "underflow of stack of procedure frames"

showIllegalInstr        :: Instr -> String
showIllegalInstr instr1
    = "unimplemented instr: " ++ showInstr instr1

showIllegalOperand      :: String -> MV -> String
showIllegalOperand err v
    = "illegal operand: "
      ++ err
      ++ " expected, but got "
      ++ showMV v

-- -------------------------------------------------------------------
