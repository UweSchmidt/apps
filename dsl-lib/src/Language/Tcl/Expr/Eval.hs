module Language.Tcl.Expr.Eval
where

import Control.Monad

import Data.Char        ( isLetter )
import Data.Maybe      	( isJust )

import           Language.Tcl.Core
import           Language.Tcl.CheckArgs          	( tclCheckBooleanArg
                                                        , tclBooleanVal
                                                        )
import           Language.Tcl.Expr.AbstractSyntax
import qualified Language.Tcl.Expr.Parser               as P

-- ------------------------------------------------------------

data Val
    = I Integer
    | D Double
    | S String

instance Show Val where
    show (I i) = show i
    show (D d) = show d
    show (S s) = s

eval :: TclExpr -> TclEval e s Val
eval (TIConst i)
    = return $ I i

eval (TFConst f)
    = return $ D f

eval (TSConst s)
    = return $ S s

eval (TExpr op [e1])
    = eval e1 >>= app1 op

eval (TExpr "&&" [e1, e2])
    = eval (TExpr "?:" [e1, e2, TIConst 0])

eval (TExpr "||" [e1, e2])
    = eval (TExpr "?:" [e1, TIConst 1, e2])

eval (TExpr op [e1, e2])
    = do v1 <- eval e1
         v2 <- eval e2
         app2 op v1 v2

eval (TExpr "?:" [e1, e2, e3])
    = do v1 <- eval e1
         b  <- tclCheckBooleanArg $ show v1
         if b
            then eval e2
            else eval e3

eval (TExpr op _el)
    = tclSyn $ op ++ " not supported"

-- ------------------------------------------------------------
--
-- unary expr evaluation

app1 :: String -> Val -> TclEval e s Val
app1 "+" x@(I _)    = return x
app1 "+" x@(D _)    = return x

app1 "-"   (I x)    = return . I $ 0 - x
app1 "-"   (D x)    = return . D $ 0 - x

app1 "!"   (I x)    = return . b2i $ x /= 0
app1 "!"   (D x)    = return . b2i $ x /= 0

app1 op v
    | isLetter . head $ op
        = tclSyn $ op ++ "(" ++ show v ++ ")"
    | otherwise
        = tclSyn $ unwords [op, show v]

-- ------------------------------------------------------------
--
-- binary expr evaluation

app2 :: String -> Val -> Val -> TclEval e s Val
app2 "+" (I x1) (I x2)  = return . I $ x1 + x2
app2 "+" (D x1) (D x2)  = return . D $ x1 + x2

app2 "-" (I x1) (I x2)  = return . I $ x1 - x2
app2 "-" (D x1) (D x2)  = return . D $ x1 - x2

app2 "*" (I x1) (I x2)  = return . I $ x1 * x2
app2 "*" (D x1) (D x2)  = return . D $ x1 * x2

app2 "/" (I x1) (I x2)
    | x2 == 0           = tclThrowError "divide by zero"
    | otherwise         = return . I $ x1 * x2
app2 "/" (D x1) (D x2)
    | x2 == 0           = tclThrowError "divide by zero"
    | otherwise         = return . D $ x1 * x2

app2 "%" (I x1) (I x2)
    | x2 == 0           = tclThrowError "modulo by zero"
    | otherwise         = return . I $ x1 * x2
app2 "%" x1 x2          = app2Type "%" x1 x2

app2 "==" (I x1) (I x2)  = return . b2i $ x1 == x2
app2 "==" (D x1) (D x2)  = return . b2i $ x1 == x2
app2 "==" (S x1) (S x2)  = return . b2i $ x1 == x2

app2 "!=" x1 x2          = app2 "==" x1 x2 >>= app2 "-" (I 1)

app2 op x1 x2
    = case castArgs x1 x2 of
        Nothing
            -> app2Syn op x1 x2		-- no casts possible: issue error
        Just (y1, y2)
            -> app2 op y1 y2		-- try again with one arg implicitly casted

castArgs :: Val -> Val -> Maybe (Val, Val)
castArgs x y
    | isJust r		= mzero
    | otherwise		= ( s2i y >>= eqarg x )		-- try string to int cast for y
                          `mplus`
                          ( s2i x >>= flip eqarg y )	-- try string to int cast for x
                          `mplus`
                          ( i2d y >>= eqarg x )		-- try int to double cast for y
                          `mplus`
                          ( i2d x >>= flip eqarg y )	-- try int to double cast for x
                          `mplus`
                          ( i2s y >>= eqarg x )		-- try int to string cast for y
                          `mplus`
                          ( i2s x >>= flip eqarg y )	-- try int to string cast for x
                          `mplus`
                          ( d2s y >>= eqarg x )		-- try double to string cast for y
                          `mplus`
                          ( d2s x >>= flip eqarg y )	-- try double to string cast for x
    where
      r = eqarg x y

eqarg :: Val -> Val -> Maybe (Val, Val)
eqarg x@(I _) y@(I _) = return (x, y)
eqarg x@(D _) y@(D _) = return (x, y)
eqarg x@(S _) y@(S _) = return (x, y)
eqarg _       _       = mzero

s2i :: Val -> Maybe Val
s2i (S s) = fmap (I . toInteger . fromEnum) . either (const mzero) return . tclBooleanVal $ s
s2i _     = mzero

i2d :: Val -> Maybe Val
i2d (I i) = return . D . fromIntegral $ i
i2d _     = mzero

i2s :: Val -> Maybe Val
i2s (I i) = return . S . show $ i
i2s _     = mzero

d2s :: Val -> Maybe Val
d2s (D x) = return . S . show $ x
d2s _     = mzero

b2i :: Bool -> Val
b2i =  I . toInteger . fromEnum

-- ------------------------------------------------------------

app2Syn, app2Type :: String -> Val -> Val -> TclEval e s r

app2Syn    = app2Err tclSyn
app2Type   = app2Err tclType

app2Err :: (Show a1, Show a) => (String -> t) -> String -> a -> a1 -> t
app2Err err op v1 v2
    | isLetter . head $ op
        = err $ op ++ "(" ++ show v1 ++ "," ++ show v2 ++ ")"
    | otherwise
        = err $ unwords [show v1, op, show v2]

tclSyn, tclType :: String -> TclEval e s r

tclSyn	= tclExprErr "syntax error in expression: "
tclType	= tclExprErr "wrong argument types in expression: "

tclExprErr :: String -> String -> TclEval e s r
tclExprErr msg
    = tclThrowError . (msg ++) . show

-- ------------------------------------------------------------

parseTclExpr	:: String -> TclEval e s TclExpr
parseTclExpr s
    = case (P.parseTclExpr s) of
        Left err
            -> tclThrowError $ show err
        Right l
            -> return l

evalTclExpr :: String -> TclEval e s String
evalTclExpr s
    = parseTclExpr s >>= eval >>= return . show

-- ------------------------------------------------------------
