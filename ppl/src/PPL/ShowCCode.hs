{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module PPL.ShowCCode
    (showCCode
    ) where

import PPL.Instructions
import PPL.ShowCode

showCCode       :: Executable -> String
showCCode (is, ds)
    = concat (map (++ "\n")
              [ "#include \"vm.h\""
              , ""
              , "static Int i_zero  = 0;"
              , "static Int i_one   = 1;"
              , "static Int i_two   = 2;"
              , "static Int i_three = 3;"
              , "static Int i_four  = 4;"
              , "static Int i_five  = 5;"
              , "static Int i_six   = 6;"
              , "static Int i_seven = 7;"
              , "static Real f_zero = 0.0;"
              , "static Real f_one  = 1.0;"
              , ""
              , cConst
              , "static Instr instructions[] ="
              , "\t{ " ++ cInstr
              , "\t};"
              , ""
              , "int main(int argc, char * argv[]) {"
              , "    return pplvm(argc, argv, instructions, "
                ++ "sizeof(instructions) / sizeof(*instructions), "
                ++ show ds
                ++ ");"
              , "}"
              ]
             )
      where
      (cInstr, cConst) = showCCode1 is

showCCode1      :: Code -> (String, String)
showCCode1 is
    = (is1, cs1)
      where (is0, cs0)  = unzip (zipWith showCInstr is [0..])
            is1         = foldr1 (\x y -> x ++ "\n\t, " ++ y) is0
            cs1         = concat cs0

showCInstr      :: Instr -> Int -> (String, String)

showCInstr (LoadI i) n
    = showOpCode1 "loadi" (showIntArg i n)

showCInstr (LoadF f) n
    = showOpCode1 "loadf" (showFloatArg f n)

showCInstr (LoadS s) n
    = showOpCode1 "loads" (showStrArg s n)

showCInstr  LoadU _
    = showOpCode0 "undef"

showCInstr  LoadEL _
    = showOpCode0 "emptyl"

showCInstr (Load (LocA a)) n
    = showOpCode1 "loadLoc" (showIntArg a n)

showCInstr (Load (AbsA a)) n
    = showOpCode1 "loadAbs" (showIntArg a n)

showCInstr (Store (LocA a)) n
    = showOpCode1 "storeLoc" (showIntArg a n)

showCInstr (Store (AbsA a)) n
    = showOpCode1 "storeAbs" (showIntArg a n)

showCInstr  Pop _
    = showOpCode0 "pop"

showCInstr  Dup _
    = showOpCode0 "dup"

showCInstr (Compute opcode) _
    = showOpCode0 (showOpCode opcode)

showCInstr (SysCall subr) _
    = showOpCode0 ("svc_" ++ subr)

showCInstr (PushJ l) n
    = showOpCode1 "pushj" (showDestArg l n)

showCInstr  PopJ _
    = showOpCode0 "popj"

showCInstr (Entry len) n
    = showOpCode1 "entry" (showIntArg len n)

showCInstr  Exit _
    = showOpCode0 "exit"

showCInstr (Branch cond l) n
    = showOpCode1 ("br" ++ (if cond then "True" else "False")) (showDestArg l n)

showCInstr (Jump l) n
    = showOpCode1 "jump" (showDestArg l n)

showCInstr (IllegalInstr s) n
    = showOpCode1 "illegalInstr" (showStrArg s n)

showCInstr (Label l) _
    = showOpCode1 "illegalInstr" (showLabelArg l)

showOpCode1 opcode (id', const')
    = ( showOpCode' opcode id', const')

showOpCode0 opcode
    = ( showOpCode' opcode "0", "")

showOpCode' opcode arg
    = "{ " ++ "op_" ++ opcode ++ "\t, " ++ arg ++ " }"

showIntArg 0 _          = ("&i_zero",   "")
showIntArg 1 _          = ("&i_one",    "")
showIntArg 2 _          = ("&i_two",    "")
showIntArg 3 _          = ("&i_three",  "")
showIntArg 4 _          = ("&i_four",   "")
showIntArg 5 _          = ("&i_five",   "")
showIntArg 6 _          = ("&i_six",    "")
showIntArg 7 _          = ("&i_seven",  "")
showIntArg i n          = ("&i_" ++ show n, "static Int i_"   ++ show n ++   " = " ++ show i ++ ";\n")

showFloatArg 0.0 _      = ("&f_zero", "")
showFloatArg 1.0 _      = ("&f_one",  "")
showFloatArg f n        = ("&f_" ++ show n, "static Real f_" ++ show n ++   " = " ++ show f ++ ";\n")

showStrArg s n          = ( "s_" ++ show n, "static char s_"  ++ show n ++ "[] = " ++ show s ++ ";\n")

showDestArg (Symb l) _n = ( "0", "#error \"label not resolved: " ++ l ++ "\"\n")
showDestArg (Disp d) n  = showIntArg d n

showLabelArg l          = ( "0", "#error \"label not resolved: " ++ l ++ "\"\n")
