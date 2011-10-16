{-# OPTIONS -fno-warn-orphans #-}

-- ------------------------------------------------------------

-- the simple functions defined on instructions

-- ------------------------------------------------------------

module Language.Lua.VM.Instr
where

import Data.Char            	( toLower )

import Language.Lua.VM.Types

-- ------------------------------------------------------------

instance Show Code where
    show (Code is) = unlines . map show $ is

-- ------------------------------------------------------------

instance Show MCode where
    show (MCode is) = showMachineCode . Code $ is

-- ------------------------------------------------------------

instance Show Instr where
    show (LoadNum d   ) = fmt1 "load" (show d)
    show (LoadStr s   ) = fmt1 "load" (show s)
    show (LoadBool b  ) = fmt1 "load" (if b then "true" else "false")
    show (LoadNil     ) = fmt1 "load" "nil"
    show (LoadEmpty   ) = fmt1 "load" "()"
    show (LoadVar n   ) = fmt1 "load" n
    show (LoadField   ) = fmt1 "load" ".[.]"
    show (NewTable    ) = fmt0 "newtab"
    show (NewEnv      ) = fmt0 "newenv"
    show (NewLocal n  ) = fmt1 "newloc" n
    show (DelEnv      ) = fmt0 "delenv"
    show (StoreVar n  ) = fmt1 "store" n
    show (StoreField  ) = fmt1 "store" ".[.]"
    show (Append      ) = fmt0 "append"
    show (MkTuple     ) = fmt0 "mktuple"
    show (UnTuple     ) = fmt0 "untuple"
    show (Take1       ) = fmt0 "take1"
    show (Pop         ) = fmt0 "pop"
    show (Copy i      ) = fmt1 "copy"  $ show i
    show (Move i      ) = fmt1 "move"  $ show i
    show (BinOp op    ) = fmt0 $ fmtOp $ show op
    show (UnOp op     ) = fmt0 $ fmtOp $ show op
    show (Jump l      ) = fmt1 "jump" (show l)
    show (Label l     ) = fmtL $ show l
    show (Branch b l  ) = fmt1 ("br" ++ if b then "true" else "false") (show l)
    show (Closure l   ) = fmt1 "closure" (show l)
    show (Call        ) = fmt0 "call"
    show (TailCall    ) = fmt0 "tailcall"
    show (Leave       ) = fmt0 "return"
    show (Intr msg    ) = fmt1 "intr" (show msg)

indent     :: String -> String
indent s   = replicate 8 ' ' ++ s

fill       :: Int -> String -> String
fill n s   = take n (s ++ replicate n ' ')

fmt0       :: String -> String
fmt0 s     = indent s

fmt1       :: String -> String -> String
fmt1 s0 s1 = indent $ fill 8 s0 ++ s1

fmtL       :: String -> String
fmtL l     = l ++ ":"

fmtOp      :: String -> String
fmtOp      = map toLower

-- ------------------------------------------------------------

showMachineCode :: Code -> String
showMachineCode (Code is)
    = unlines . map (uncurry showMachineInstr) $ zip [0..] is

showMachineInstr :: Int -> Instr -> String
showMachineInstr ic instr
    = fmtCnt 4 ++ fmtInstr
    where
      fmtCnt n = (++ replicate (8 - n) ' ') .
                 reverse . take n . reverse .
                 ((replicate n ' ') ++) .
                 show
                 $ ic
      fmtInstr
          = drop 8 $ show instr ++ target instr
          where
            dist                d   = "   --> " ++ show (d + ic)
            target (Jump     (D d)) = dist d
            target (Branch _ (D d)) = dist d
            target (Closure  (D d)) = dist d
            target _                = ""

-- ------------------------------------------------------------

mkInstr     :: Instr -> Code
mkInstr     = Code . (:[])

branch      :: Bool -> Label -> Instr
branch c l  = Branch c (M l)

jump        :: Label -> Instr
jump        = Jump . M

closure     :: Label -> Instr
closure     = Closure . M

-- ------------------------------------------------------------

