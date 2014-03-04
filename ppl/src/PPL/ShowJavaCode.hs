module PPL.ShowJavaCode
    (showJavaCode
    ) where

import PPL.Instructions
import PPL.ShowCode

showJavaCode    :: String -> Executable -> String
showJavaCode className (is, ds)
    = concat (map (++ "\n")
              [ "package ppl;"
              , ""
              , "import ppl.Executable;"
              , ""
              , "public class " ++ className ++ " extends Executable {"
              , "    public " ++ className ++ "() {"
              , "        super(new Instr []"
              , "              { " ++ showJCode is
              , "              }"
              , "             , " ++ show ds
              , "             );"
              , "        }"
              , "    }"
              ]
             )

showJCode       :: Code -> String
showJCode       = foldr1 (\ x y -> x ++ "\n              , " ++ y) . map showJInstr

showJInstr      :: Instr -> String

showJInstr (LoadI i)
    = "loadi(" ++ show i ++ ")"

showJInstr (LoadF f)
    = "loadf(" ++ show f ++ ")"

showJInstr (LoadS s)
    = "loads(" ++ show s ++ ")"

showJInstr  LoadU
    = "undef()"

showJInstr  LoadEL
    = "emptyl()"

showJInstr (Load addr)
    = "load(" ++ showJAddr addr ++ ")"

showJInstr (Store addr)
    = "store(" ++ showJAddr addr ++ ")"

showJInstr  Pop
    = "pop()"

showJInstr  Dup
    = "dup()"

showJInstr (Compute opcode)
    = "compute(" ++ showOpCode opcode ++ ")"

showJInstr (SysCall subr)
    = "svc(" ++ subr ++ ")"

showJInstr (PushJ l)
    = "pushj(" ++ showJDest l ++ ")"

showJInstr  PopJ
    = "popj()"

showJInstr (Entry len)
    = "entry(" ++ show len ++ ")"

showJInstr  Exit
    = "exit()"

showJInstr (Branch cond l)
    = "branch(" ++ (if cond then "true" else "false") ++ "," ++ showJDest l ++ ")"

showJInstr (Jump l)
    = "jump(" ++ showJDest l ++ ")"

showJInstr (IllegalInstr s)
    = "illegalInstr(" ++ show s ++ ")"

showJInstr (Label l)
    = "illegalInstr(" ++ showJLabel l ++ ")"

{-
showJInstr _
    = "illegalInstr(\"unknown instruction\")"
-}

showJDest               :: Dest -> String
showJDest (Symb l)      = showJLabel l
showJDest (Disp d)      = show d

showJLabel              :: String -> String
showJLabel l            = "label not resolved: " ++ l

showJAddr               :: Address -> String
showJAddr (LocA a)      = "locAddr," ++ show a
showJAddr (AbsA a)      = "absAddr," ++ show a
