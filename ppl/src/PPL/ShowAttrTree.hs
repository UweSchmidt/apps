module PPL.ShowAttrTree where

import PPL.AbstractSyntax
import PPL.NTree

showAttrTree    :: AttrTree -> String
showAttrTree    = formatStringNTree . atree2NTree

-- -------------------------------------------------------------------

atree2NTree     :: AttrTree -> StringNTree

atree2NTree (Opr f al, t)
    = NTree ("\"" ++ f ++ "\" (" ++ show t ++ ")" ) (map atree2NTree al)

atree2NTree (e, t)
    = NTree (showSExpr e ++ " (" ++ show t ++ ")" ) []

-- -------------------------------------------------------------------

showSExpr               :: Expr -> String

showSExpr (IntVal v)    = show v
showSExpr (BoolVal v)   = show v
showSExpr (FloatVal v)  = show v
showSExpr (StringVal v) = "\"" ++ show v ++ "\""
showSExpr (EmptyList)   = "[]" 
showSExpr (Ident i)     = "\"id\" " ++ i
showSExpr e             = show e
