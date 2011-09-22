-- ------------------------------------------------------------

-- the instruction set of this Lua VM

-- ------------------------------------------------------------

module Language.Lua.VM.Instr
    ( BOp(..)
    , UOp(..)
    , Instr(..)
    , Label(..)
    , Dest(..)
    , Code(..)
    , mkInstr
    , branch
    , jump
    , closure
    )
where

import Data.Char           ( toLower )
import Data.Monoid

-- ------------------------------------------------------------

type Name
    = String

data BOp
    = Add | Sub | Mult | Div | Exp | Mod | EQU | NEQ | GRT | GRE | LSE | LST | Conc
      deriving (Show, Eq, Ord)

data UOp
    = Minus | Not | NumberOf
      deriving (Show, Eq, Ord)

data Instr
    = LoadNum Double
    | LoadStr String
    | LoadBool Bool
    | LoadNil
    | LoadEmpty		-- load empty result list
    | LoadVar Name
    | LoadField         -- top: the table, 2.: the index
    | NewTable
    | NewEnv            -- create a new empty env (on block or function entry)
    | DelEnv            -- remove an env on block exit (normal exit, break and return)
    | NewLocal Name     -- create new variable in local env
    | StoreVar Name
    | StoreField        -- top: the table, 2.: the index, 3. the value
    | Append            -- top: the table, 2.: the value(-list)
    | MkTuple		-- top: the tail (single value or list), 2. the head value
    | UnTuple           -- top: the head or nil, 2. the rest, maybe the empty tuple
    | Take1             -- top: take the head or nil, discard the rest
    | Pop               -- throw away the topmost value
    | Copy Int          -- push the i. value onto the stack, 0 == top of stack, stack growes by 1
    | Move Int          -- move the i. value to the top, i == 1: swap the 2 topmost values, stack remains size
    | BinOp BOp         -- top: right arg, 2.: left arg
    | UnOp UOp          -- top: single arg
    | Jump Dest
    | Label Label
    | Branch Bool Dest  -- pop top of stack and test on (false or nil)
    | Closure Dest      -- push a closure
    | Call              -- top: closure, 2. args
    | TailCall          -- top: closure, 2. args
    | Leave             -- remove current env and return to saved stored closure
    | Exit Int          -- terminate prog
    | TODO String	-- for debugging

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
    show (Exit rc     ) = fmt1 "exit" (show rc)
    show (TODO s      ) = fmt1 "TODO" s

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

data Dest
    = L Label
    | D Int

instance Show Dest where
    show (L l) = show l
    show (D d) = show d

-- ------------------------------------------------------------

newtype Label
    = Lab Int
      deriving (Eq, Ord)

instance Show Label where
    show (Lab l) = format l
        where
          format = ("l" ++) . reverse . take 3 . reverse . ("0000" ++) . show
               
-- ------------------------------------------------------------

newtype Code
    = Code [Instr]

instance Show Code where
    show (Code is) = "\n" ++ concatMap ((++ "\n") . show) is

instance Monoid Code where
    mempty = Code []
    mappend (Code c1) (Code c2) = Code $ c1 ++ c2

mkInstr :: Instr -> Code
mkInstr = Code . (:[])

branch :: Bool -> Label -> Instr
branch c l = Branch c (L l)

jump :: Label -> Instr
jump = Jump . L

closure :: Label -> Instr
closure = Closure . L

-- ------------------------------------------------------------
