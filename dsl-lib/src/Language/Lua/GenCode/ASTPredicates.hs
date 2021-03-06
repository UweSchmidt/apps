module Language.Lua.GenCode.ASTPredicates
where

import Data.Maybe                   ( isNothing )
import Language.Lua.AST

-- ------------------------------------------------------------

--
-- predicates for AST

isSingleResExpr                      :: Expr -> Bool
isSingleResExpr (EEllipsis)          = False
isSingleResExpr (ECall _ _)          = False
isSingleResExpr (EMemberCall _ _ _)  = False
isSingleResExpr _                    = True

hasLocalDefs                         :: [Stmt] -> Bool
hasLocalDefs                         = any isLocalDef

noFctParams                          :: Expr -> Bool
noFctParams (EFunction [] False _)   = True
noFctParams _                        = False

isLocalDef                           :: Stmt -> Bool
isLocalDef (SLocalDef _ _)           = True
isLocalDef _                         = False

isReturnBlock                        :: Block -> Bool
isReturnBlock (Block sl)             = hasReturn sl

isReturnStmt                         :: Stmt -> Bool
isReturnStmt (SReturn _ )            = True

isReturnStmt (SDo block)             = isReturnBlock block

isReturnStmt (SIf tps ep)            = all (isReturnBlock . snd) tps
                                       &&
                                       maybe False isReturnBlock ep

isReturnStmt _                       = False

hasReturn                            :: [Stmt] -> Bool
hasReturn []                         = False
hasReturn sl                         = isReturnStmt (last sl)

isTailCall                           :: [Expr] -> Bool
isTailCall [ECall _ _]               = True
isTailCall [EMemberCall _ _ _]       = True
isTailCall _                         = False

noOfAppends                          :: [(Maybe Expr, Expr)] -> Int
noOfAppends                          = length . filter (isNothing . fst)

isEllipsis                           :: LValue -> Bool
isEllipsis (LVar "...")              = True
isEllipsis _                         = False

isFieldRef                           :: LValue -> Bool
isFieldRef (LFieldRef _ _)           = True
isFieldRef _                         = False

-- ------------------------------------------------------------
