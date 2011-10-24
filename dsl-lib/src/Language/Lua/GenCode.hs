module Language.Lua.GenCode
where

import Language.Lua.AST
import Language.Lua.GenCode.ASTPredicates
import Language.Lua.GenCode.State

import Language.Lua.VM.Instr
import Language.Lua.VM.Types

import Control.Applicative ( (<$>) )
import Control.Monad.RWS

-- ------------------------------------------------------------

compileProg :: Block -> (Code, CErrs)
compileProg block = runCompile (compProg block)

-- ------------------------------------------------------------

compProg :: Block -> Compile ()
compProg block
    = do start     <- newLabel
         emitCode $ mkInstr $ jump start
         code_prog <- compExpr prog
         code_cls  <- genCode [ mkInstr $ Label start
                              , mkInstr $ LoadEmpty	-- push an empty param tuple
                              , code_prog		-- push the closure
                              , mkInstr $ Call
                              , mkInstr $ Pop
                              , mkInstr $ Intr "terminate program"
                              ]
         emitCode code_cls
         return ()
    where
      prog = EFunction [] False block

compBlock :: Block -> Compile Code
compBlock (Block sl)
    | hasLocalDefs sl
        = compWithLocalEnv $ compStmtL sl
    | otherwise			-- optimization: unnecessay env creation thrown away
        = compStmtL sl

compStmtL :: [Stmt] -> Compile Code
compStmtL sl
    = mconcat <$> mapM compStmt sl

-- ------------------------------------------------------------

compStmt :: Stmt -> Compile Code

compStmt (SLocalDef lv f@[EFunction _ _ _])
    = do c1 <- compStmt (SLocalDef lv [])
         c2 <- compStmt (SAssignment (map LVar lv) f)
         genCode [c1, c2]

compStmt (SLocalDef lvs [])
    = compNewLocals lvs

compStmt (SLocalDef [lv] [e])
    = do c1 <- compExpr1  e
         nv <- compNewLocals [lv]
         c2 <- compStore mempty (LVar lv)
         genCode [c1, nv, c2]

compStmt (SLocalDef lvs es)
    = do c1 <- compExprL  es
         c2 <- compStoreLocals lvs
         genCode [c1, c2]

compStmt (SAssignment [lv] [e])
    = do c1 <- compExpr1 e
         c2 <- compStore mempty lv
         genCode [c1, c2]

compStmt (SAssignment lvs es)
    = do c1 <- compExprL  es
         c2 <- compStoreL lvs
         genCode [c1, c2]

compStmt (SIf thenParts elsePart)
    = do lEnd <- newLabel
         (code_thps, end_thps) <- compThenParts lEnd thenParts
         maybe ( genCode [ code_thps
                         , mkInstr $ Label end_thps
                         , mkInstr $ Label lEnd
                         ]
               ) (\ ep ->
                      do code_elsePart <- compBlock ep
                         genCode [ code_thps
                                 , mkInstr $ jump lEnd
                                 , mkInstr $ Label end_thps
                                 , code_elsePart
                                 , mkInstr $ Label lEnd
                                 ]
                 ) elsePart
    where
      compThenParts _ (tp : [])
          = compThenPart tp

      compThenParts lEnd (tp : tps)
          = do (code_tp,  lab_tp ) <- compThenPart  tp
               (code_tps, lab_tps) <- compThenParts lEnd tps
               code <- genCode [ code_tp
                               , mkInstr $ jump  lEnd
                               , mkInstr $ Label lab_tp
                               , code_tps
                               ]
               return (code, lab_tps)

      compThenParts lEnd []
          = return (mempty, lEnd)

      compThenPart (cond, block)
          = do lCont <- newLabel
               code_cond  <- compCond False lCont cond
               code_block <- compBlock block
               code       <- genCode [code_cond, code_block]
               return (code, lCont)

compStmt (SWhile cond body)
    = do lBody <- newLabel
         lCond <- newLabel
         lEnd  <- newLabel
         codeCond <- compCond True lBody cond
         codeBody <- compWithNewLoopLevel lEnd $
                     compBlock body
         genCode [ mkInstr $ jump  lCond
                 , mkInstr $ Label lBody
                 , codeBody
                 , mkInstr $ Label lCond
                 , codeCond
                 , mkInstr $ Label lEnd
                 ]

compStmt (SUntil cond body)
    = do lBody <- newLabel
         lEnd  <- newLabel
         codeCond <- compCond False lBody cond
         codeBody <- compWithNewLoopLevel lEnd $
                     compBlock body
         genCode [ mkInstr $ Label lBody
                 , codeBody
                 , codeCond
                 , mkInstr $ Label lEnd
                 ]

compStmt SBreak
    = getLoopLevel >>=
      maybe (do cerr "compiling break statement" "no surrounding loop found"
                return mempty
            ) (\ (i, lab) -> genCode $ replicate i (mkInstr DelEnv)
                                       ++
                                       [mkInstr $ jump lab]
              )

compStmt (SReturn es)
    | isTailCall es
        = compTailCall $ head es
    | otherwise
        = do code_es  <- compExprL es
             genCode [ code_es
                     , mkInstr Leave
                     ]

compStmt (SDo block)
    = compBlock block

compStmt (SFor [v] (ForNum e1 e2 e3) block)
    = compBlock forBlock
    where
      var          = "$var"		-- generated variables
      limit        = "$limit"
      step         = "$step"
      var'         = EVar var
      limit'       = EVar limit
      step'        = EVar step
      zero'        = ENumber 0.0
      one'         = ENumber 1.0
      tonumber' e  = ECall  (EVar "tonumber") [e]
      not'         = EUnOp  "not"
      and'         = EBinOp "and"
      or'          = EBinOp "or"
      gr'          = EBinOp ">"
      ge'          = EBinOp ">="
      le'          = EBinOp "<="
      plus'        = EBinOp "+"
      error'       = SAssignment []
                                 [ECall  (EVar "error")
                                             [EString "missing number(s) in for loop var, limit or step"]
                                 ]
      forBlock
          = Block [ SLocalDef [var]   [tonumber' e1]
                  , SLocalDef [limit] [tonumber' e2]
                  , SLocalDef [step]  [tonumber' . maybe one' id $ e3]
                  , SIf [( (not' (var' `and'` (limit' `and'` step')))
                         , Block [error']
                         )
                        ] Nothing
                  , SWhile ( ( (step' `gr'` zero') `and'` (var' `le'` limit')   )
                             `or'`
                             ( (step' `le'` zero') `and'` (var' `ge'` limit') )
                           ) ( Block [ SLocalDef [v] [var']
                                     , SDo block
                                     , SAssignment [LVar var] [var' `plus'` step']
                                     ]
                             )
                  ]

{- a for loop is transformed 1-1 into a while loop
   applying the schema given in the Lua reference manual

   Lua reference manual:
   A for statement like

     for v = e1, e2, e3 do block end

  is equivalent to the code:
  
     do
       local var, limit, step = tonumber(e1), tonumber(e2), tonumber(e3)
       if not (var and limit and step) then error() end
       while (step > 0 and var <= limit) or (step <= 0 and var >= limit) do
         local v = var
         block
         var = var + step
       end
     end
-}

compStmt (SFor _ (ForNum _ _ _) _)
    = error "compStmt: error in AST: single loop variable required for counting loops"

compStmt (SFor vs@(v1 : _) (ForIter explist) block)
    = compBlock forBlock
    where
      f         = "$f"		-- generated variables
      s         = "$s"
      var       = "$var"
      f'        = EVar f
      s'        = EVar s
      var'      = EVar var
      true'     = EBool True
      eqNil' e  = EBinOp "==" e ENil
      f''       = ECall f'
      forBlock
          = Block [ SLocalDef [f, s, var] explist
                  , SWhile true'
                           ( Block [ SLocalDef vs [f'' [s', var']]
                                   , SAssignment [LVar var] [EVar v1]
                                   , SIf [(eqNil' var', Block [SBreak])] Nothing
                                   , SDo block
                                   ]
                           )
                  ]
{-
  A for statement like

     for var_1, ···, var_n in explist do block end

  is equivalent to the code:

     do
       local f, s, var = explist
       while true do
         local var_1, ···, var_n = f(s, var)
         var = var_1
         if var == nil then break end
         block
       end
     end

-}

compStmt (SFor _ (ForIter _) _)
    = error "compStmt: error in AST: loop variable required for general loops"

{- compStmt is complete

compStmt s
    = todo "compStmt" (show s)
-}
-- ------------------------------------------------------------

compNewLocals :: [Name] -> Compile Code
compNewLocals lvs
    = genCode $ map (mkInstr . NewLocal) lvs

compStoreLocals :: [Name] -> Compile Code
compStoreLocals lvs
    = do nv <- compNewLocals lvs
         c2 <- compStoreL (map LVar lvs)
         genCode [nv, c2]

compStoreL :: [LValue] -> Compile Code
compStoreL []
    = genCode [ mkInstr Pop ]

compStoreL lvs
    = do code_lvs' <- mapM compStore1 lvs'
         code_lv   <- compStoreLast $ head lv
         genCode $ code_lvs'
                   ++
                   [ code_lv ]
  where
    len1 = length lvs - 1
    (lvs', lv) = splitAt len1 lvs

compStore1 :: LValue -> Compile Code
compStore1
    = compStore $ mkInstr UnTuple

compStoreLast :: LValue -> Compile Code
compStoreLast lv
    | isEllipsis lv
        = compStore mempty lv
    | otherwise
        = compStore (mkInstr Take1) lv
    where
      isEllipsis (LVar "...") = True
      isEllipsis _            = False

-- gen code for storing a single value from the eval stack into a variable
-- into a field of a table, in the 1 case stack size is decreased by 1
-- in the 2. case the top value is a ref to the table, the 2. the field and
-- the 3. the value, so the eval stack is decreased by 3

compStore :: Code -> LValue -> Compile Code
compStore instr (LVar n)
    = genCode [ instr
              , mkInstr $ StoreVar n
              ]

compStore instr (LFieldRef tb ix)
    = do code_ix <- compExpr1 ix
         code_tb <- compExpr1 tb
         genCode [ instr
                 , code_ix
                 , code_tb
                 , mkInstr StoreField
                 ]

compWithLocalEnv :: Compile Code -> Compile Code
compWithLocalEnv compPart
    = do part <- local incrEnvCnt compPart
         genCode [ mkInstr NewEnv
                 , part
                 , mkInstr DelEnv
                 ]

compWithNewEnv :: Compile Code -> Compile Code
compWithNewEnv compPart
    = local resetEnvCnt compPart

compWithNewLoopLevel :: Label -> Compile Code -> Compile Code
compWithNewLoopLevel lEnd compPart
    = local (newLoopLevel lEnd) compPart

-- ------------------------------------------------------------

-- gen code such that the list of expressions is pushed as a single tuple of values onto
-- the evaluation stack

compExprL :: [Expr] -> Compile Code
compExprL []
    = return $ mkInstr $ LoadEmpty
compExprL es
    = do code_es' <- mapM compExpr1 es'
         code_le  <- compExpr (head le)
         genCode $ code_es'
                   ++
                   [ code_le ]
                   ++
                   replicate len1 (mkInstr MkTuple)
      where
        len1 = length es -1
        (es', le) = splitAt len1 es

-- gen code such that a single value, not a tuple is pushed onto the eval stack
-- from tuple values the 1. one (or nil) will be taken

compExpr1 :: Expr -> Compile Code
compExpr1 e
    | isSingleResExpr e
        = compExpr e
    | otherwise
        = do code_e <- compExpr e
             genCode [ code_e
                     , mkInstr Take1
                     ]

-- gen code such that a single value is pushed onto the evaluation stack
-- this value may be a tuple of arbitrary length

compExpr :: Expr -> Compile Code
compExpr (ENumber d)
    = genCode [mkInstr $ LoadNum d]

compExpr (EString s)
    = genCode [mkInstr $ LoadStr s]

compExpr (EBool b)
    = genCode [mkInstr $ LoadBool b]

compExpr (ENil)
    = genCode [mkInstr $ LoadNil]

compExpr (EEllipsis)
    = genCode [mkInstr $ LoadVar "..."]

compExpr (EVar n)
    = genCode [mkInstr $ LoadVar n]

compExpr (EFieldRef table key)
    = do code_key   <- compExpr1 key
         code_table <- compExpr1 table
         genCode [ code_key
                 , code_table
                 , mkInstr $ LoadField
                 ]

compExpr e@(EBinOp op e1 e2)
    | op `elem` ["and", "or"]
        = compAndOr e
    | otherwise
        = do code_e1 <- compExpr1 e1
             code_e2 <- compExpr1 e2
             instr   <- compOpcode
             genCode [ code_e1
                     , code_e2
                     , instr
                     ]
    where
      compOpcode
          = maybe ( todo "unimplemented" (" binary op " ++ show op) )
                  ( \ o -> genCode [ mkInstr $ BinOp o ] ) $
            lookup op optable
          where
            optable = [ ("+",  Add)
                      , ("-",  Sub)
                      , ("*",  Mult)
                      , ("/",  Div)
                      , ("^",  Exp)
                      , ("%",  Mod)
                      , ("==", EQU)
                      , ("~=", NEQ)
                      , (">",  GRT)
                      , (">=", GRE)
                      , ("<=", LSE)
                      , ("<",  LST)
                      , ("..", Conc)
                      ]

compExpr (EUnOp op e1)
    | op == "-"					-- (- e) = (0 - e)
        = compExpr (EBinOp op (ENumber 0.0) e1)
    | op == "#"
        = do code_e1 <- compExpr e1
             genCode [ code_e1
                     , mkInstr $ UnOp NumberOf
                     ]
    | op == "not"
        = do code_e1 <- compExpr e1
             l1      <- newLabel
             l2      <- newLabel
             genCode [ code_e1
                     , mkInstr $ branch False l1
                     , mkInstr $ LoadBool True
                     , mkInstr $ jump l2
                     , mkInstr $ Label l1
                     , mkInstr $ LoadBool False
                     , mkInstr $ Label l2
                     ]
    | otherwise
        = todo "unimplemented" (" unary op " ++ show op)

compExpr (ECall fct args)
    = compCall (mkInstr Call) fct args

-- Lua reference: arguments are evaluated before function
-- this needs some swapping on the stack, because the table is used
-- twice, once as a extra 1. arg and as lookup table for the method

compExpr (EMemberCall table name args)
    = compMemberCall (mkInstr Call) table name args

compExpr (EFunction fps varargs block)
    = do lStart <- newLabel
         code_f <- compWithNewEnv $
                   compWithLocalEnv $
                   do code_par  <- compStoreLocals fps'
                      code_body <- compBlock block
                      genCode [ code_par
                              , code_body
                              ]
         code_e <- exitCode
         code_c <- genCode [ mkInstr $ Label lStart
                           , code_f
                           , code_e
                           ]
         emitCode code_c
         genCode [ mkInstr $ closure lStart ]
    where
      fps'
          | varargs   = fps ++ ["..."]
          | otherwise = fps

      exitCode
          | isReturnBlock block
              = genCode []
          | otherwise
              = genCode [ mkInstr $ LoadEmpty
                        , mkInstr $ Leave
                        ]

compExpr (ETableCons fs)
    = do code_es <- compFieldList fs
         genCode [ mkInstr $ NewTable
                 , code_es
                 ]
    where
      compField compEx key val
          = maybe (compAppend compEx val)
                  (compStoreField val)
                  key

      compAppend compEx val
          = do code_val <- compEx val
               genCode [ code_val
                       , mkInstr $ Copy 1
                       , mkInstr $ Append
                       ]

      compStoreField val key
          = do code_val <- compExpr1 val
               code_key <- compExpr1 key
               genCode [ code_val
                       , code_key
                       , mkInstr $ Copy 2
                       , mkInstr $ StoreField
                       ]
      
      compFieldList []
          = genCode []
      compFieldList [fLast]
          = uncurry (compField compExpr) fLast		-- last field must be compiled with compExpr
      compFieldList (f1 : fs')                          -- all others with compExpr1
          = do code_f1  <- uncurry (compField  compExpr1) f1
               code_fs' <- compFieldList fs'
               genCode [code_f1, code_fs']

{- compExpr is complete

compExpr e
   = todo "compExpr" $ show e
-}

-- ------------------------------------------------------------

compTailCall :: Expr -> Compile Code
compTailCall (ECall fct args)
    = compCall (mkInstr TailCall) fct args

compTailCall (EMemberCall table name args)
    = compMemberCall (mkInstr TailCall) table name args

compTailCall _
    = error "compTailCall: illegal arg, not a call expr"

-- ------------------------------------------------------------

compCall :: Code -> Expr -> [Expr] -> Compile Code
compCall callInstr fct args
    = do code_args <- compExprL args
         code_fct  <- compExpr1 fct
         genCode [ code_args
                 , code_fct
                 , callInstr
                 ]

compMemberCall :: Code -> Expr -> String -> [Expr] -> Compile Code
compMemberCall callInstr table name args
    = do code_table <- compExpr1 table
         code_args  <- compExprL args
         genCode [ code_args                    -- evaluate args first
                 , code_table			-- eval table, this value is used twice
                 , mkInstr $ Copy 0             -- so copy it
                 , mkInstr $ Move 2             -- move the arg list to the top
                 , mkInstr $ MkTuple            -- and cons the table as 1. arg in front of the arg list
                 , mkInstr $ LoadStr name       -- load table index
                 , mkInstr $ Move 2             -- move table to the top
                 , mkInstr $ LoadField          -- compute the function (method) to be called by table lookup
                 , callInstr                    -- everthing is ready for calling the function
                 ]

-- ------------------------------------------------------------

-- gen code such that a single value is pushed onto the stack,
-- this value will be consumed by a conditional branch instr
-- so after executing the sequence, the stack has the same state as before
--
-- compCond can be used to compile an expression from statement level
-- where the expression is used as a condition for branching
--
-- extra cases for compiling "and", "or" and "not" are inserted
-- to gen simpler code as with compExpr
---
-- in the context of a condition (in if, while, ...) and only there
--   not (not e) == e

compCond :: Bool -> Label -> Expr -> Compile Code
compCond c l (EUnOp "not" e1)
    = compCond (not c) l e1

compCond c l (EBinOp "or" e1 e2)		-- de Morgan
    = compCond (not c) l (EBinOp "and" (not' e1) (not' e2))
    where
      not' = EUnOp "not"

compCond c l (EBinOp "and" e1 e2)
    | c
        = do nl      <- newLabel
             code_e1 <- compCond False nl e1
             code_e2 <- compCond True  l  e2
             genCode [ code_e1
                     , code_e2
                     , mkInstr $ Label nl
                     ]
    | otherwise
        = do code_e1 <- compCond False l  e1
             code_e2 <- compCond False l  e2
             genCode [ code_e1
                     , code_e2
                     ]

compCond c l e
    = do codeExpr <- compExpr e
         genCode [ codeExpr
                 , mkInstr $ branch c l
                 ]

compAndOr :: Expr -> Compile Code
compAndOr (EBinOp op e1 e2)
    = do code_e1 <- compExpr1 e1
         code_e2 <- compExpr1 e2
         lEnd    <- newLabel
         genCode [ code_e1
                 , mkInstr $ Copy 0
                 , mkInstr $ branch (op == "or") lEnd
                 , mkInstr $ Pop
                 , code_e2
                 , mkInstr $ Label lEnd
                 ]
compAndOr _
    = error "compAndOr: called with illegal expr"

-- ------------------------------------------------------------
--
-- mothers little helpers

todo :: String -> String -> Compile Code
todo s msg
    = do emitErr msg'
         return $ mkInstr $ Intr msg'
    where
      msg' = unwords ["TODO:", s, msg]

cerr :: (Show a) => String -> a -> Compile ()
cerr s msg
    = emitErr $ unwords ["ERROR", "in", s ++ ":", show msg]

genCode :: [Code] -> Compile Code
genCode = return . mconcat

-- ------------------------------------------------------------
--
-- basic env, state and writer operations

newLabel :: Compile Label
newLabel
    = modify genNewLabel >>
      gets theNewLabel

getLoopLevel :: Compile (Maybe (Int, Label))
getLoopLevel
    = asks theLoopLevel

getEnvCnt :: Compile Int
getEnvCnt
    = asks theEnvCnt

-- ------------------------------------------------------------
