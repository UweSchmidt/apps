-- ------------------------------------------------------------

-- the instruction set of this Lua VM

-- ------------------------------------------------------------

module Language.Lua.Instr
    ( BOp(..)
    , UOp(..)
    , Instr(..)
    , Label(..)
    , AInstr
    , MInstr
    , ACode
    , MCode
    , mkInstr
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

data Instr lab
    = LoadNum Double
    | LoadStr String
    | LoadBool Bool
    | LoadNil
    | LoadEmpty		-- load empty result list
    | LoadVar Name
    | LoadField         -- binary op
    | NewTable
    | NewEnv            -- create a new empty env (on block or function entry)
    | DelEnv            -- remove an env on block exit (normal exit, break and return)
    | NewLocal Name     -- create new variable in local env
    | StoreVar Name
    | StoreField        -- store a value under a key in a table
    | Append            -- store a value at the next "free" position in a table
    | MkTuple		-- cons the 2. top value with the tol value list
    | UnTuple           -- split top level value list into head and tail
    | Take1             -- take the head of a value list and discard the tail
    | Pop               -- throw away the topmost value
    | Dup Int           -- duplicate topmost value
    | Swap              -- swap the 2 topmost values
    | BinOp BOp
    | UnOp UOp
    | Jump lab
    | Label lab
    | Branch Bool lab
    | Closure lab
    | Call
    | TailCall
    | Leave
    | Exit Int
    | TODO String	-- for debugging

instance (Show lab) => Show (Instr lab) where
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
    show (Dup i       ) = fmt1 "dup" $ show i
    show (Swap        ) = fmt0 "swap"
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

newtype Label = Lab Int

instance Show Label where
    show (Lab l) = format l
        where
          format = ("l" ++) . reverse . take 3 . reverse . ("0000" ++) . show
               
type AInstr = Instr Label
type ACode  = Code  Label

type MInstr = Instr Int
type MCode  = Code  Int

-- ------------------------------------------------------------

newtype Code a
    = Code [Instr a]

instance (Show a) => Show (Code a) where
    show (Code is) = "\n" ++ concatMap ((++ "\n") . show) is

instance Monoid (Code a) where
    mempty = Code []
    mappend (Code c1) (Code c2) = Code $ c1 ++ c2

mkInstr :: Instr a -> Code a
mkInstr = Code . (:[])

-- ------------------------------------------------------------
