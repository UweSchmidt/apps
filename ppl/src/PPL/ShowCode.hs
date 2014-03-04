module PPL.ShowCode where

import PPL.Instructions

showExecutable  :: Executable -> String
showExecutable (is, ds)
    = ".text\n" ++ showCode is ++ "\n" ++ showDS ds

showExecutable1 :: Executable -> String
showExecutable1 (is, ds)
    = "        .text\n" ++ showCode1 is ++ "\n        " ++ showDS ds

showDS          :: DataSeg -> String
showDS ds
    = ".data\t" ++ show ds ++ "\n"

showCode        :: Code -> String
showCode        = concat . map showInstr

showCode1       :: Code -> String
showCode1       = concat . zipWith showInstrCnt [0..]

showInstrCnt    :: Int -> Instr -> String
showInstrCnt cnt ins'
    = reverse (take 8 (reverse ("       " ++ show cnt ++ ":")))
      ++ showInstr' ins'
    where
    showTarget d
        = "\t\t--> " ++ show (cnt + d) ++ "\n"
    showInstr' ins@(Branch _ (Disp d))
        = init (showInstr ins) ++ showTarget d
    showInstr' ins@(Jump (Disp d))
        = init (showInstr ins) ++ showTarget d
    showInstr' ins@(PushJ (Disp d))
        = init (showInstr ins) ++ showTarget d
    showInstr' ins
        = showInstr ins

showInstr       :: Instr -> String
showInstr (LoadI i)
    = "\tloadi\t" ++ show i ++ "\n"

showInstr (LoadF f)
    = "\tloadf\t" ++ show f ++ "\n"

showInstr (LoadS s)
    = "\tloads\t" ++ show s ++ "\n"

showInstr  LoadU
    = "\tundef\n"

showInstr  LoadEL
    = "\temptyl\n"

showInstr (Load addr)
    = "\tload\t" ++ showAddr addr ++ "\n"

showInstr (Store addr)
    = "\tstore\t" ++ showAddr addr ++ "\n"

showInstr  Pop
    = "\tpop\n"

showInstr  Dup
    = "\tdup\n"

showInstr (Compute opcode)
    = "\t" ++ showOpCode opcode ++ "\n"

showInstr (SysCall subr)
    = "\t" ++ "svc" ++ "\t" ++ subr ++ "\n"

showInstr (PushJ l)
    = "\tpushj\t" ++ showDest l ++ "\n"

showInstr  PopJ
    = "\tpopj\n"

showInstr (Entry len)
    = "\tentry\t" ++ show len ++ "\n"

showInstr  Exit
    = "\texit\n"

showInstr (Branch cond l)
    = "\tbr" ++ (if cond then "true" else "false") ++ "\t" ++ showDest l ++ "\n"

showInstr (Jump l)
    = "\tjmp\t" ++ showDest l ++ "\n"

showInstr (IllegalInstr s)
    = "\t" ++ "ERROR:\t" ++ s ++ "\n"

showInstr (Label l)
    = showLabel l ++ ":\n"

showComment     :: String -> String
showComment ""  = ""
showComment s   = "\t\t-- " ++ s

showDest        :: Dest -> String
showDest (Symb l)       = showLabel l
showDest (Disp d)       = show d

showLabel       :: Label -> String
showLabel l = l

showAddr        :: Address -> String
showAddr (LocA a)       = "l[" ++ show a ++ "]"
showAddr (AbsA a)       = "m[" ++ show a ++ "]"

showOpCode      :: Opcode -> String
showOpCode op
    = drop 2 (show op)
