module PPL.ShowAbstractSyntaxTree where

import PPL.AbstractSyntax
import PPL.NTree

showAST         :: Program -> String
showAST         = formatStringNTree . ast2NTree

ast2NTree       :: Program -> NTree String
ast2NTree (Program gdl st)
    = NTree "Program" (map stmt2NTree gdl ++ [stmt2NTree st])

-- -------------------------------------------------------------------

stmt2NTree      :: Stmt -> StringNTree
stmt2NTree (Assignment vl el)
    = NTree "Assignment" (map expr2NTree vl ++ map expr2NTree el)

stmt2NTree (Decl v t)
    = NTree "Decl" [expr2NTree v, type2NTree t]

stmt2NTree (FctDecl fn pl t body)
    = NTree "FctDecl" ( [ expr2NTree fn ]
                        ++
                        map stmt2NTree pl
                        ++ 
                        [ type2NTree t
                        , expr2NTree body
                        ]
                      )

stmt2NTree (ProcDecl fn pl body)
    = NTree "ProcDecl" ( [ expr2NTree fn ]
                         ++
                         map stmt2NTree pl
                         ++ 
                         [stmt2NTree body]
                      )

stmt2NTree (ProcCall e)
    = NTree "ProcCall" [expr2NTree e]

stmt2NTree (Block sl)
    = NTree "Block" (map stmt2NTree sl)

stmt2NTree (While e s)
    = NTree "While" [expr2NTree e, stmt2NTree s]

stmt2NTree (Repeat s e)
    = NTree "Repeat" [stmt2NTree s, expr2NTree e]

stmt2NTree (If e s1 s2)
    = NTree "If" [expr2NTree e, stmt2NTree s1, stmt2NTree s2]

-- stmt2NTree s                 -- default rule
--     = NTree (show s) []

-- -------------------------------------------------------------------

expr2NTree      :: Expr -> StringNTree
expr2NTree (IntVal v)
    = NTree ("IntVal " ++ show v) []

expr2NTree (BoolVal v)
    = NTree ("BoolVal " ++ show v) []

expr2NTree (FloatVal v)
    = NTree ("FloatVal " ++ show v) []

expr2NTree (StringVal v)
    = NTree ("StringVal \"" ++ v ++ "\"") []

expr2NTree (Ident i)
    = NTree ("Ident " ++ i) []

expr2NTree (Call f al)
    = NTree ("Op " ++ f) (map expr2NTree al)

expr2NTree (BlockExpr sl re)
    = NTree "BlockExpr " (map stmt2NTree sl ++ [expr2NTree re])

-- default rule
expr2NTree e
    = NTree (show e) []

-- -------------------------------------------------------------------

type2NTree      :: Type -> NTree String

type2NTree (ListType t)
    = NTree ("ListType") [type2NTree t]

type2NTree (FctType rt ats)
    = NTree ("FctType") (map type2NTree (rt:ats))

type2NTree e
    = NTree (show e) []
