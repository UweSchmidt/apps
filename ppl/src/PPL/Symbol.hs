module PPL.Symbol where

data Token
    = Eof
    | IdentSy           -- e.g. i1
    | StringConst       -- e.g. "hahahahah\"jajaj"
    | BoolConst         -- false or true
    | IntConst          -- e.g. 123
    | FloatConst        -- e.g. 123.456, 123., .456
    | AndSy             -- and
    | OrSy              -- or
    | XorSy             -- xor
    | NotSy             -- not
    | BeginSy           -- begin
    | DivSy             -- div
    | ModSy             -- mod
    | MinSy             -- min
    | MaxSy             -- max
    | IfSy              -- if
    | ThenSy            -- then
    | ElseSy            -- else
    | ElseIfSy          -- elseif
    | WhileSy           -- while
    | DoSy              -- do
    | RepeatSy          -- repeat
    | UntilSy           -- until
    | EndSy             -- end
    | EndIfSy           -- endif
    | EndWhileSy        -- endwhile
    | ReturnSy          -- return
    | OfSy              -- of
    | VarSy             -- var
    | FunctionSy        -- function
    | ProcedureSy       -- procedure
    | IntSy             -- int
    | FloatSy           -- real
    | BoolSy            -- boolean
    | StringSy          -- string
    | PicSy             -- picture
    | ListSy            -- list
    | LPar              -- (
    | RPar              -- )
    | LBr               -- [
    | RBr               -- ]
    | Assign            -- :=
    | Colon             -- :
    | Semicolon         -- ;
    | Comma             -- ,
    | Dot               -- .
    | PlusOp            -- +
    | MinusOp           -- -
    | MultOp            -- *
    | DivOp             -- /
    | EqOp              -- =
    | NeOp              -- /=
    | GeOp              -- >=
    | GrOp              -- >
    | LeOp              -- <=
    | LtOp              -- <
    | EquivOp           -- <=>
    | ImplOp            -- =>
    | Illegal           -- everything else
      deriving Show

type Symbol = (Token, String)
