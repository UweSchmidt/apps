module Language.Lua.Compile
where

import Language.Lua.AST
import Language.Lua.CompileState
import Language.Lua.Instr

import Control.Applicative ( (<$>) )
import Control.Monad.RWS

-- ------------------------------------------------------------

compileProg :: Block -> (ACode, CErrs)
compileProg block = runCompile (compProg block)

-- ------------------------------------------------------------

compProg :: Block -> Compile ()
compProg block
    = do start     <- newLabel
         emitCode $ mkInstr $ Jump start
         code_prog <- compExpr prog
         code_cls  <- genCode [ mkInstr $ Label start
                              , mkInstr $ LoadEmpty
                              , code_prog
                              , mkInstr $ Call
                              , mkInstr $ Pop
                              , mkInstr $ Exit 0
                              ]
         emitCode code_cls
         return ()
    where
      prog = EFunction [] False block

compBlock :: Block -> Compile ACode
compBlock (Block sl)
    | hasLocalDefs sl
        = compWithLocalEnv $ compStmtL sl
    | otherwise			-- optimization: unnecessay env creation thrown away
        = compStmtL sl

compStmtL :: [Stmt] -> Compile ACode
compStmtL sl
    = mconcat <$> mapM compStmt sl

-- ------------------------------------------------------------

compStmt :: Stmt -> Compile ACode

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
                                 , mkInstr $ Jump lEnd
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
                               , mkInstr $ Jump  lEnd
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
         genCode [ mkInstr $ Jump  lCond
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
                                       [mkInstr $ Jump lab]
              )

compStmt (SReturn es)
    = do l        <- getEnvCnt
         code_es  <- compExprL es
         code_env <- genCode $ replicate l (mkInstr DelEnv)
         genCode [ code_es
                 , code_env
                 , exitCode
                 ]
    where
      tailCall [ECall _ _] = True
      tailCall _           = False

      exitCode
          | tailCall es    = mkInstr $ TailCall
          | otherwise      = mkInstr $ Leave

compStmt (SDo block)
    = compBlock block

compStmt (SFor [v] (ForNum e1 e2 e3) block)
    = compBlock forBlock
    where
      var          = "$var"
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
    = do cerr "for statement" "single loop variable required"
         genCode []


compStmt s
    = todo "compStmt" (show s)

-- ------------------------------------------------------------

compNewLocals :: [Name] -> Compile ACode
compNewLocals lvs
    = genCode $ map (mkInstr . NewLocal) lvs

compStoreLocals :: [Name] -> Compile ACode
compStoreLocals lvs
    = do nv <- compNewLocals lvs
         c2 <- compStoreL (map LVar lvs)
         genCode [nv, c2]

compStoreL :: [LValue] -> Compile ACode
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

compStore1 :: LValue -> Compile ACode
compStore1
    = compStore $ mkInstr UnTuple

compStoreLast :: LValue -> Compile ACode
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

compStore :: ACode -> LValue -> Compile ACode
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

compWithLocalEnv :: Compile ACode -> Compile ACode
compWithLocalEnv compPart
    = do part <- local incrEnvCnt compPart
         genCode [ mkInstr NewEnv
                 , part
                 , mkInstr DelEnv
                 ]

compWithNewEnv :: Compile ACode -> Compile ACode
compWithNewEnv compPart
    = local resetEnvCnt compPart

compWithNewLoopLevel :: Label -> Compile ACode -> Compile ACode
compWithNewLoopLevel lEnd compPart
    = local (newLoopLevel lEnd) compPart

-- ------------------------------------------------------------

-- gen code such that the list of expressions is pushed as a single list of values onto
-- the evaluation stack

compExprL :: [Expr] -> Compile ACode
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

compExpr1 :: Expr -> Compile ACode
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

compExpr :: Expr -> Compile ACode
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
                     , mkInstr $ Branch False l1
                     , mkInstr $ LoadBool True
                     , mkInstr $ Jump l2
                     , mkInstr $ Label l1
                     , mkInstr $ LoadBool False
                     , mkInstr $ Label l2
                     ]
    | otherwise
        = todo "unimplemented" (" unary op " ++ show op)

compExpr (ECall fct args)
    = do code_fct  <- compExpr1 fct
         code_args <- compExprL args
         genCode [ code_fct
                 , code_args
                 , mkInstr $ Call
                 ]

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
         genCode [ mkInstr $ Closure lStart ]
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
                       , mkInstr $ Dup 1
                       , mkInstr $ Append
                       ]

      compStoreField val key
          = do code_val <- compExpr1 val
               code_key <- compExpr1 key
               genCode [ code_val
                       , code_key
                       , mkInstr $ Dup 2
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

compExpr e
    = todo "compExpr" $ show e

-- gen code such that a single value is pushed onto the stack,
-- this value will be consumed by a conditional branch instr
-- so after executing the sequence, the stack has the same state as before
--
-- extra cases for compiling "and" and "or" may be inserted
-- to gen simpler code as with compExpr

compCond :: Bool -> Label -> Expr -> Compile ACode
compCond c l e
    = do codeExpr <- compExpr e
         genCode [ codeExpr
                 , mkInstr $ Branch c l
                 ]

compAndOr :: Expr -> Compile ACode
compAndOr (EBinOp op e1 e2)
    = do code_e1 <- compExpr1 e1
         code_e2 <- compExpr1 e2
         lEnd    <- newLabel
         genCode [ code_e1
                 , mkInstr $ Dup 0
                 , mkInstr $ Branch (op == "or") lEnd
                 , mkInstr $ Pop
                 , code_e2
                 , mkInstr $ Label lEnd
                 ]
compAndOr _
    = error "compAndOr: called with illegal expr"

-- ------------------------------------------------------------
--
-- mothers little helpers

todo :: String -> String -> Compile ACode
todo s msg
    = do emitErr $ unwords ["TODO:", s, msg]
         return $ mkInstr $ TODO $ msg

cerr :: (Show a) => String -> a -> Compile ()
cerr s msg
    = emitErr $ unwords ["ERROR", "in", s ++ ":", show msg]

genCode :: [ACode] -> Compile ACode
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
--
-- predicates for AST

isSingleResExpr                      :: Expr -> Bool
isSingleResExpr (EEllipsis)          = False
isSingleResExpr (ECall _ _)          = False
isSingleResExpr (EMemberCall _ _ _)  = False
isSingleResExpr _                    = True

hasLocalDefs                         :: [Stmt] -> Bool
hasLocalDefs                         = any isLocalDef

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


-- ------------------------------------------------------------
