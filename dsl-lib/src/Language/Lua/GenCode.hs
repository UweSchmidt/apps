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
         emitCode $ mkJump start
         code_prog <- compExpr prog
         code_cls  <- genCode [ mkLabel start
                              , mkEmptyTup 		-- push an empty param tuple
                              , code_prog		-- push the closure
                              , mkCall
                              , mkPop
                              , mkIntr "terminate program"
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
compStmtL = compSeq compStmt

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
                         , mkLabel end_thps
                         , mkLabel lEnd
                         ]
               ) (\ ep ->
                      do code_elsePart <- compBlock ep
                         genCode [ code_thps
                                 , mkJump lEnd
                                 , mkLabel end_thps
                                 , code_elsePart
                                 , mkLabel lEnd
                                 ]
                 ) elsePart
    where
      compThenParts _ (tp : [])
          = compThenPart tp

      compThenParts lEnd (tp : tps)
          = do (code_tp,  lab_tp ) <- compThenPart  tp
               (code_tps, lab_tps) <- compThenParts lEnd tps
               code <- genCode [ code_tp
                               , mkJump  lEnd
                               , mkLabel lab_tp
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
         genCode [ mkJump  lCond
                 , mkLabel lBody
                 , codeBody
                 , mkLabel lCond
                 , codeCond
                 , mkLabel lEnd
                 ]

compStmt (SUntil cond body)
    = do lBody <- newLabel
         lEnd  <- newLabel
         codeCond <- compCond False lBody cond
         codeBody <- compWithNewLoopLevel lEnd $
                     compBlock body
         genCode [ mkLabel lBody
                 , codeBody
                 , codeCond
                 , mkLabel lEnd
                 ]

compStmt SBreak
    = getLoopLevel >>=
      maybe (do cerr "compiling break statement" "no surrounding loop found"
                return mempty
            ) (\ (i, lab) -> genCode [ mkDelEnvN i
                                     , mkJump lab
                                     ]
              )

compStmt (SReturn es)
    | isTailCall es
        = compTailCall $ head es
    | otherwise
        = do code_es  <- compExprL es
             genCode [ code_es
                     , mkLeave
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

compStmt s@(SFor _ (ForNum _ _ _) _)
    = cerror "compStmt" ("error in AST: single loop variable required for counting loops:" ++ show s)

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

compStmt s@(SFor _ (ForIter _) _)
    = cerror "compStmt" ("error in AST: loop variable required for general loops: " ++ show s)

{- compStmt is complete

compStmt s
    = todo "compStmt" (show s)
-}
-- ------------------------------------------------------------

compNewLocals :: [Name] -> Compile Code
compNewLocals lvs
    = genCode $ map mkNewLocal lvs

compStoreLocals :: [Name] -> Compile Code
compStoreLocals lvs
    = do nv <- compNewLocals lvs
         c2 <- compStoreL (map LVar lvs)
         genCode [nv, c2]

compStoreL :: [LValue] -> Compile Code
compStoreL []
    = genCode [ mkPop ]

-- {- new tuple instructions

compStoreL lvs
    = do code_lvs <- compSeq (compStore mempty) lvs
         genCode [ mkUnT (length lvs)
                 , code_lvs
                 ]
    where
      mkUnT
          | isEllipsis (last lvs) = mkUnTupN'
          | otherwise             = mkUnTupN

-- -}
{- old tuple instructions

compStoreL lvs
    = do code_lvs' <- mapM compStore1 lvs'
         code_lv   <- compStoreLast $ head lv
         genCode $ code_lvs'
                   ++
                   [ code_lv ]
  where
    len1 = length lvs - 1
    (lvs', lv) = splitAt len1 lvs
-- -}

compStore1 :: LValue -> Compile Code
compStore1
    = compStore $ mkUnTup2

compStoreLast :: LValue -> Compile Code
compStoreLast lv
    | isEllipsis lv
        = compStore mempty lv
    | otherwise
        = compStore mkUnTup1 lv

-- gen code for storing a single value from the eval stack into a variable
-- into a field of a table, in the 1 case stack size is decreased by 1
-- in the 2. case the top value is a ref to the table, the 2. the field and
-- the 3. the value, so the eval stack is decreased by 3

compStore :: Code -> LValue -> Compile Code
compStore instr (LVar n)
    = genCode [ instr
              , mkStoreVar n
              ]

compStore instr (LFieldRef tb ix)
    = do code_ix <- compExpr1 ix
         code_tb <- compExpr1 tb
         genCode [ instr
                 , code_ix
                 , code_tb
                 , mkStoreField
                 ]

compWithLocalEnv :: Compile Code -> Compile Code
compWithLocalEnv compPart
    = do part <- local incrEnvCnt compPart
         genCode [ mkNewEnv
                 , part
                 , mkDelEnv
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
    = return mkEmptyTup

compExprL es
    = do code_es' <- compSeq compExpr1 es'
         code_le  <- compExpr (head le)
         genCode $ [ code_es'
                   , code_le
                   , mkTupN len
                   ]
      where
        len  = length es
        len1 = len -1
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
                     , mkUnTup1
                     ]

-- gen code such that a single value is pushed onto the evaluation stack
-- this value may be a tuple of arbitrary length

compExpr :: Expr -> Compile Code
compExpr (ENumber d)
    = genCode [mkLoadNum d]

compExpr (EString s)
    = genCode [mkLoadStr s]

compExpr (EBool b)
    = genCode [mkLoadBool b]

compExpr (ENil)
    = genCode [mkLoadNil]

compExpr (EEllipsis)
    = genCode [mkLoadVar "..."]

compExpr (EVar n)
    = genCode [mkLoadVar n]

compExpr (EFieldRef table key)
    = do code_key   <- compExpr1 key
         code_table <- compExpr1 table
         genCode [ code_key
                 , code_table
                 , mkLoadField
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
                  ( \ o -> genCode [ mkBinOp o ] ) $
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
                     , mkUnOp NumberOf
                     ]
    | op == "not"
        = do code_e1 <- compExpr e1
             l1      <- newLabel
             l2      <- newLabel
             genCode [ code_e1
                     , mkBranch False l1
                     , mkLoadBool True
                     , mkJump l2
                     , mkLabel l1
                     , mkLoadBool False
                     , mkLabel l2
                     ]
    | otherwise
        = todo "unimplemented" (" unary op " ++ show op)

compExpr (ECall fct args)
    = compCall mkCall fct args

-- Lua reference: arguments are evaluated before function
-- this needs some swapping on the stack, because the table is used
-- twice, once as a extra 1. arg and as lookup table for the method

compExpr (EMemberCall table name args)
    = compMemberCall mkCall table name args

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
         code_c <- genCode [ mkLabel lStart
                           , code_f
                           , code_e
                           ]
         emitCode code_c
         genCode [ mkClosure lStart ]
    where
      fps'
          | varargs   = fps ++ ["..."]
          | otherwise = fps

      exitCode
          | isReturnBlock block
              = genCode []
          | otherwise
              = genCode [ mkEmptyTup
                        , mkLeave
                        ]

compExpr (ETableCons fs)
    = do code_es <- compFieldList 0 fs
         code_ap <- codeAppends (noOfAppends fs)
         genCode $ [ mkNewTable
                   , code_es
                   , code_ap
                   ]
    where
      codeAppends 0
          = genCode []
      codeAppends n
          = genCode [ mkTupN n
                    , mkCopy 1
                    , mkAppendField
                    ]

      compField n compEx key val
          = maybe (compAppend compEx val)
                  (compStoreField n val)
                  key

      compAppend compEx val
          = compEx val

      compStoreField n val key
          = do code_val <- compExpr1 val
               code_key <- compExpr1 key
               genCode [ code_val
                       , code_key
                       , mkCopy (n + 2)
                       , mkStoreField
                       ]
      
      compFieldList _ []
          = genCode []

      compFieldList n [fLast]
          = uncurry (compField n compExpr) fLast	-- last field must be compiled with compExpr

      compFieldList n (f1 : fs')                        -- all others with compExpr1
          = do code_f1  <- uncurry (compField n compExpr1) f1
               code_fs' <- compFieldList (n1 $ fst f1) fs'
               genCode [code_f1, code_fs']
          where
            n1 Nothing = n + 1
            n1 _       = n

{- compExpr is complete

compExpr e
   = todo "compExpr" $ show e
-}

-- ------------------------------------------------------------

compTailCall :: Expr -> Compile Code
compTailCall (ECall fct args)
    = compCall mkTailCall fct args

compTailCall (EMemberCall table name args)
    = compMemberCall mkTailCall table name args

compTailCall e
    = cerror "compTailCall" ("illegal arg, not a call expr: " ++ show e)

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
                 , mkCopy 0                     -- so copy it
                 , mkMove 2                     -- move the arg list to the top
                 , mkTupN 2                     -- and cons the table as 1. arg in front of the arg list
                 , mkLoadStr name               -- load table index
                 , mkMove 2                     -- move table to the top
                 , mkLoadField                  -- compute the function (method) to be called by table lookup
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
                     , mkLabel nl
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
                 , mkBranch c l
                 ]

compAndOr :: Expr -> Compile Code
compAndOr (EBinOp op e1 e2)
    = do code_e1 <- compExpr1 e1
         code_e2 <- compExpr1 e2
         lEnd    <- newLabel
         genCode [ code_e1
                 , mkCopy 0
                 , mkBranch (op == "or") lEnd
                 , mkPop
                 , code_e2
                 , mkLabel lEnd
                 ]
compAndOr e
    = cerror "compAndOr:" ("called with illegal expr: " ++ show e)

-- ------------------------------------------------------------
--
-- mothers little helpers

todo :: String -> String -> Compile Code
todo = compErr "Codegen TODO:"

cerror :: String -> String -> Compile Code
cerror = compErr "Codegen internal error:"

compErr :: String -> String -> String -> Compile Code
compErr what s msg
    = do emitErr msg'
         return $ mkIntr msg'
    where
      msg' = unwords [what, s, msg]

cerr :: (Show a) => String -> a -> Compile ()
cerr s msg
    = emitErr $ unwords ["ERROR", "in", s ++ ":", show msg]

genCode :: [Code] -> Compile Code
genCode = return . mconcat

compSeq :: (a -> Compile Code) -> [a] -> Compile Code
compSeq comp xs = mconcat <$> mapM comp xs

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
