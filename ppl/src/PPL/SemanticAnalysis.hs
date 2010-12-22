
module PPL.SemanticAnalysis where

import PPL.AbstractSyntax
import PPL.BuiltinFunctions

checkProg       :: Program -> AttrTree
checkProg       = checkProg' globalEnv

type Env        = [NameSpace]
type NameSpace  = [(String, Descr)]

data Descr
    = VarDescr  Type
    | FctDescr  Type    FKind
      deriving (Eq, Show)

data FKind
    = SvcFct                            -- not yet used
    | UserDef [ParamDecl] FctBody
      deriving (Eq, Show)


checkProg' :: Env -> Program -> AttrTree

checkProg' env (Program gdl st)
    = (Opr "sequence" (st' : gdl'), VoidType)
      where
      env1 = newEnv env gdl
      st'  = checkStmt env1 st
      gdl' = map (checkGlobalDecl env1) gdl

checkGlobalDecl :: Env -> GlobDecl -> AttrTree
checkGlobalDecl env (FctDecl fn parlist resType body)
    = (Opr "fctdecl" (fn' : body' : parlist'), resType)
      where
      env1      = newEnv env parlist
      fn'       = (fn, VoidType)
      body'     = checkExpr env1 resType body
      parlist'  = map (checkStmt env1) parlist

checkGlobalDecl env (ProcDecl fn parlist body)
    = (Opr "fctdecl" (fn' : body' : parlist'), VoidType)
      where
      env1      = newEnv env parlist
      fn'       = (fn, VoidType)
      body'     = checkStmt env1 body
      parlist'  = map (checkStmt env1) parlist


checkGlobalDecl env st
    = checkStmt env st

checkStmt       :: Env -> Stmt -> AttrTree

checkStmt env (Assignment vs es)
    | length vs /= length es
        = error ( "# of variables in left hand side "
                  ++ "of assignment does not match # of expressions"
                )
    | otherwise
        = let
          vs1 = map (typeExpr env) vs
          ts1 = map snd vs1
          es1 = zipWith (checkExpr env) ts1 es
          in
          (Opr ":=" (vs1 ++ es1), VoidType)

checkStmt env (Block sl)
    = let
      (_env1, stmtl, undefl) = buildEnv env sl
      in
      (Opr "begin" [(Opr "sequence" (stmtl ++ undefl)
                    , VoidType)]
      , VoidType)

checkStmt env (Decl v@(Ident id') _)
    = (Opr "decl" [(v,t)], VoidType)
      where
      VarDescr t        = getVarDescr id' env

checkStmt env (ProcCall e)
    = (Opr "do" [e'], VoidType)
    where
    e' = checkExpr env VoidType e

checkStmt env (While e s)
    = (Opr "while" [e', s'], VoidType)
    where
    e' = checkExpr env BoolType e
    s' = checkStmt env s

checkStmt env (Repeat s e)
    = (Opr "repeat" [s', e'], VoidType)
    where
    e' = checkExpr env BoolType e
    s' = checkStmt env s

checkStmt env (If e s1 s2)
    = (Opr "if" [e', s1', s2'], VoidType)
    where
    e'  = checkExpr env BoolType e
    s1' = checkStmt env s1
    s2' = checkStmt env s2

checkStmt _env _stmt
    = error "compiler error: illegal statement"

-- -------------------------------------------------------------------
-- simple environment
-- all block local env are stored in a list
-- head contains local variable descriptions
-- tail contains global variable descriptions

newEnv          :: Env -> [Stmt] -> Env
newEnv env dl
    = insDecl ([]:env) dl

insDecl         :: Env -> [Stmt] -> Env
insDecl env []
    = env

insDecl env ((Decl (Ident id') t):dl)
    = insDecl (insId env id' (varDescr t)) dl

insDecl env ((FctDecl (Ident fn) pl rt body):dl)
    = insDecl (insId env fn (fctDescr pl rt body)) dl

insDecl env ((ProcDecl (Ident fn) pl body):dl)
    = insDecl (insId env fn (fctDescr pl VoidType (BlockExpr [body] UndefVal))) dl

insDecl env (_:dl)
    = insDecl env dl

insId           :: Env -> String -> Descr -> Env
insId env id' descr
    | alreadyDefined id'
        = error ("identifier " ++ id' ++ " defined twice")
    | otherwise
        = newenv
    where
    (locenv:globenv)    = env
    alreadyDefined id'' = not . null . (lookupId id'') $ [locenv]
    newlocenv           = (id', descr) : locenv
    newenv              = newlocenv : globenv

varDescr        :: Type -> Descr
varDescr t      = VarDescr t

fctDescr        :: [ParamDecl] -> ResType -> FctBody -> Descr
fctDescr pl rt body
    = FctDescr fctType (UserDef pl body)
      where
      fctType
          = FctType rt (map paramType pl)
      paramType (Decl _ t)
          = t
      paramType _
          = error "compiler error: illegal parameter declaration"

getVarDescr     :: String -> Env -> Descr
getVarDescr id' env
    | isVarDesc d       = d
    | otherwise         = error ( "identifier is not a variable: "
                                  ++ id' )
    where
    d   = getDescr id' env

isVarDesc       :: Descr -> Bool
isVarDesc (VarDescr _)  = True
isVarDesc _             = False

getFctDescr     :: String -> Env -> Descr
getFctDescr id' env
    | isFctDesc d       = d
    | otherwise         = error ( "identifier is not a function: "
                                  ++ id' )
    where
    d   = getDescr id' env

isFctDesc       :: Descr -> Bool
isFctDesc (FctDescr _ _)= True
isFctDesc _             = False

getDescr        :: String -> Env -> Descr
getDescr id' env
    | null ids
        = error ("undeclared identifier " ++ id')
    | otherwise
        = descr
    where
    ids         = lookupId id' env
    (_, descr)  = head ids

lookupId        :: String -> Env -> NameSpace
lookupId id' env
    = filter ( \(id1,_) -> id1 == id') (concat env)

isDeclared      :: String -> Env -> Bool
isDeclared id'
    = not . null .lookupId id'

-- -------------------------------------------------------------------

checkExpr       :: Env -> Type -> Expr -> AttrTree

checkExpr env rt e
    = let
      e' = typeExpr env e
      in
      convertExpr' rt e'

convertExpr'    :: Type -> AttrTree -> AttrTree
convertExpr' rt e@(_, t)
    | re == illegalConversion
        = error ("type conflict in expression, got \""
                 ++ show t
                 ++ "\", but \""
                 ++ show rt
                 ++ "\" expected")
    | otherwise
        = re
    where
    re = convertExpr rt e


typeExpr        :: Env -> Expr -> AttrTree

typeExpr _ e@(UndefVal)
    = (e, AnyType)

typeExpr _ e@(IntVal _)
    = (e, IntType)

typeExpr _ e@(BoolVal _)
    = (e, BoolType)

typeExpr _ e@(FloatVal _)
    = (e, FloatType)

typeExpr _ e@(StringVal _)
    = (e, StringType)

typeExpr _ e@(EmptyList)
    = (e, ListType AnyType)

typeExpr env e@(Ident id')
    = (e, t)
    where
    VarDescr t = getVarDescr id' env

typeExpr env (Call fn args)
    | isDeclared fn env
      = (Opr "definedfct" (fne : (check rt'')), rt)
        where
        (FctDescr (FctType rt atypes) _fctBody)
            = getFctDescr fn env

        fne = (StringVal fn, StringType)

        args' = map (typeExpr env) args
        (args'', rt'') = opTypes rt atypes args'

        check UnknownType
            = error ("type mismatch of arguments in call of "
                     ++ fn)
        check _
            = args''
    

typeExpr env (Call fn args)
    = (Opr fn'' args'', resType)
      where
      args'
          = map (typeExpr env) args
      (fn'', (args'', resType))
          = lookupOp fn args'

typeExpr env (BlockExpr sl re)
    = let
      (env1, stmtl, undefl) = buildEnv env sl
      tre@(_e, t) = typeExpr env1 re
      in
      (Opr "begin" [(Opr "sequence" ( stmtl ++ [tre] ++ undefl )
                    , t)]
      , t)

typeExpr _env _expr
    = error "compiler error: illegal expression"

buildEnv        :: Env -> [Stmt] -> (Env, [AttrTree], [AttrTree])
buildEnv env sl
    = let
                                -- take all declaration from list sl
      dl                = filter isDecl sl
      isDecl (Decl _ _) = True
      isDecl _          = False
                                -- compute the new local environment
      env1              = newEnv env dl
                                -- construct the deallocation assignments
                                -- every variable is assigned with undef
                                -- on block exit
      undefl            = map (undefVar env1) dl
      undefVar env' (Decl v@(Ident _id) _)
          = (Opr ":=" [ typeExpr env' v
                      , (UndefVal, vt)
                      ], VoidType)
            where
            ve = typeExpr env' v
            vt = snd ve
      undefVar _env' _e
          = error "compiler error: in undefVar"
      in
      (env1, (map (checkStmt env1) sl), undefl)

-- -------------------------------------------------------------------

lookupOp        :: String -> [AttrTree] -> (String, ([AttrTree], Type))
lookupOp fn argl
    = evalRes (lookupOps fn argl)
      where
      evalRes (res:_)
          = res
      evalRes []
          = error ("function undefined or illegal argument types: " ++ show fn)

lookupOps       :: String -> [AttrTree] -> [(String, ([AttrTree], Type))]

lookupOps fn argl
    = matchtypes
    where
                                                -- lookup fct name
    fcts        = filter (\(fn1,_) -> fn1 == fn) opTypesTable
                                                -- check arguments
    fcttypes    = map (\(_, (fn', tf')) -> (fn', tf' argl)) fcts
                                                -- filter type clashes
    matchtypes  = filter ((/= noTypeMatch) . snd) fcttypes


noTypeMatch     :: ([AttrTree], Type)
noTypeMatch     = ([], UnknownType)

opTypes         :: Type -> [Type] -> [AttrTree] -> ([AttrTree], Type)
opTypes rt ts args
    | length ts /= length args
        = noTypeMatch
    | match
        = (args', rt)
    | otherwise
        = noTypeMatch
    where
    args' = zipWith convertExpr ts args
    match = and . map ( \(_,t) -> t /= UnknownType) $ args'

naryFct         :: Int -> Type -> [AttrTree] -> ([AttrTree], Type)
naryFct n t     = opTypes t (replicate n t)

naryPred        :: Int -> Type -> [AttrTree] -> ([AttrTree], Type)
naryPred n t    = opTypes BoolType (replicate n t)

nullaryFct      :: Type -> [AttrTree] -> ([AttrTree], Type)
unaryFct        :: Type -> [AttrTree] -> ([AttrTree], Type)
binaryFct       :: Type -> [AttrTree] -> ([AttrTree], Type)

nullaryFct      = naryFct 0
unaryFct        = naryFct 1
binaryFct       = naryFct 2

unaryPred       :: Type -> [AttrTree] -> ([AttrTree], Type)
binaryPred      :: Type -> [AttrTree] -> ([AttrTree], Type)

unaryPred       = naryPred 1
binaryPred      = naryPred 2

concTypes       :: [AttrTree] -> ([AttrTree], Type)

concTypes argl@[(_e1, ListType t1), (_e2, ListType t2)]
    | t1 == t2
      ||
      t1 == AnyType
      ||
      t2 == AnyType
        = (argl, ListType (commonType t1 t2))
    | otherwise
        = noTypeMatch

concTypes _argl
    = error "compiler error: in function concTypes"

commonType      :: Type -> Type -> Type

commonType AnyType t2   = t2
commonType t1 _         = t1

consTypes       :: [AttrTree] -> ([AttrTree],Type)

consTypes argl@[(e1,ListType t1),a2@(_e2, t2)]
    | t1 == t2
        = (argl, ListType t1)
    | t1 == AnyType
        = ([(e1, ListType t2), a2], ListType t2)

consTypes _
    = noTypeMatch


listType        :: Type -> [AttrTree] -> ([AttrTree],Type)

listType rt argl@[(_, ListType _)]
    = (argl, rt)

listType _ _
    = noTypeMatch


listType'       :: [AttrTree] -> ([AttrTree],Type)

listType' argl@[(_, lt@(ListType t))]
    | t == AnyType
        = error "illegal operation with empty list"
    | otherwise
        = (argl, lt)

listType' _
    = noTypeMatch


headType        :: [AttrTree] -> ([AttrTree],Type)

headType argl
    | res == noTypeMatch
        = res
    | otherwise
        = (argl', et)
    where
    res = listType' argl
    (argl', ListType et) = res


atType          :: [AttrTree] -> ([AttrTree],Type)
atType argl@[(_, ListType t),(_, IntType)]
    | t == AnyType
        = error "illegal operation with empty list"
    | otherwise
        = (argl, t)

atType _
    = noTypeMatch

ifListTypes     :: [AttrTree] -> ([AttrTree],Type)
ifListTypes argl@[ (_e0, BoolType)
                 , (_e1, ListType t1)
                 , (_e2, ListType t2)]
    | t1 == t2
      ||
      t1 == AnyType
      ||
      t2 == AnyType
        = (argl, ListType (commonType t1 t2))
    | otherwise
        = noTypeMatch

ifListTypes _argl
    = error "compiler error: in ifListTypes"

-- -------------------------------------------------------------------

-- implicit type conversions

convertExpr     :: Type -> AttrTree -> AttrTree

convertExpr rt e@(_, t)
    | rt == t   = e

convertExpr FloatType e@(_, IntType)
    = (Opr "i2f" [e], FloatType)

convertExpr t@(ListType _) (e, ListType AnyType)
    = (e, t)

convertExpr t (e, AnyType)
    = (e, t)

convertExpr _ (_, _)
    = illegalConversion

illegalConversion       :: AttrTree
illegalConversion       = (UndefVal, UnknownType)

globalEnv               :: Env
globalEnv               = []

-- -------------------------------------------------------------------

-- build in operations and functions

opTypesTable    :: [(String, (String, [AttrTree] -> ([AttrTree], Type)))]
opTypesTable =
    [ ("+",     ("addi",        intIntToInt))           -- arithmetic ops
    , ("+",     ("addf",        floatFloatToFloat))
    , ("+",     ("concs",       strStrToStr))           -- string concatenation
    , ("+",     ("mean",        picPicToPic))           -- arithm mean of colours

    , ("-",     ("subi",        intIntToInt))
    , ("-",     ("subf",        floatFloatToFloat))
    , ("-",     ("diff",        picPicToPic))           -- difference of pixels

    , ("-u",    ("negi",        intToInt))
    , ("-u",    ("negf",        floatToFloat))
    , ("-u",    ("invertp",     picToPic))

    , ("+u",    ("ident",       intToInt))
    , ("+u",    ("ident",       floatToFloat))
    , ("+u",    ("ident",       picToPic))

    , ("*",     ("muli",        intIntToInt))
    , ("*",     ("mulf",        floatFloatToFloat))
    , ("*",     ("mulp",        picPicToPic))


    , ("/",     ("divf",        floatFloatToFloat))
    , ("div",   ("divi",        intIntToInt))
    , ("mod",   ("modi",        intIntToInt))

    , ("min",   ("mini",        intIntToInt))
    , ("min",   ("minf",        floatFloatToFloat))
    , ("min",   ("minp",        picPicToPic))
    , ("max",   ("maxi",        intIntToInt))
    , ("max",   ("maxf",        floatFloatToFloat))
    , ("max",   ("maxp",        picPicToPic))
                                                        -- boolean ops
    , ("and",   ("and",         boolBoolToBool))
    , ("or",    ("or",          boolBoolToBool))
    , ("xor",   ("xor",         boolBoolToBool))
    , ("=>",    ("impl",        boolBoolToBool))
    , ("<=>",   ("equiv",       boolBoolToBool))
    , ("not",   ("not",         boolToBool))

                                                        -- compare ops
    , ("=",     ("eqi",         intIntToBool))
    , ("=",     ("eqf",         floatFloatToBool))
    , ("=",     ("eqs",         strStrToBool))
    , ("/=",    ("nei",         intIntToBool))
    , ("/=",    ("nef",         floatFloatToBool))
    , ("/=",    ("nes",         strStrToBool))

    , (">",     ("gti",         intIntToBool))
    , (">",     ("gtf",         floatFloatToBool))
    , (">=",    ("gei",         intIntToBool))
    , (">=",    ("gef",         floatFloatToBool))
    , ("<",     ("lti",         intIntToBool))
    , ("<",     ("ltf",         floatFloatToBool))
    , ("<=",    ("lei",         intIntToBool))
    , ("<=",    ("lef",         floatFloatToBool))

                                        -- conversion ops
    , ("trunc",         ("trunc",       floatToInt))
    , ("round",         ("round",       floatToInt))
    , ("toString",      ("b2s",         boolToStr))
    , ("toString",      ("i2s",         intToStr))
    , ("toString",      ("f2s",         floatToStr))

                                        -- list operations

                                        -- list concatenation
    , ("+",             ("concl",       concTypes))
    , ("cons",          ("consl",       consTypes))
    , ("append",        ("appendl",     consTypes))
    , ("empty",         ("isemptyl",    listType BoolType))
    , ("length",        ("lengthl",     listType IntType))
    , ("head",          ("headl",       headType))
    , ("tail",          ("taill",       listType'))
    , ("[.]",           ("indexl",      atType))

                                        -- conditional expression
    , ("if",            ("if",          ifInt))
    , ("if",            ("if",          ifFloat))
    , ("if",            ("if",          ifString))
    , ("if",            ("if",          ifPicture))
    , ("if",            ("if",          ifListTypes))

    ]
    ++
    buildinOps
    where
                                                -- unary
    boolToBool          = unaryFct      BoolType
    boolToStr           = opTypes       StringType [BoolType]
    -- intToFloat       = opTypes       FloatType [IntType]
    intToInt            = unaryFct      IntType
    intToStr            = opTypes       StringType [IntType]
    floatToFloat        = unaryFct      FloatType
    floatToInt          = opTypes       IntType    [FloatType]
    floatToStr          = opTypes       StringType [FloatType]
    picToPic            = unaryFct      PictureType
                                                -- binary
    boolBoolToBool      = binaryFct     BoolType
    intIntToBool        = binaryPred    IntType
    intIntToInt         = binaryFct     IntType
    floatFloatToBool    = binaryPred    FloatType
    floatFloatToFloat   = binaryFct     FloatType
    picPicToPic         = binaryFct     PictureType
    strStrToBool        = binaryPred    StringType
    strStrToStr         = binaryFct     StringType
    ifInt               = opTypes       IntType     [BoolType, IntType,     IntType]
    ifFloat             = opTypes       FloatType   [BoolType, FloatType,   FloatType]
    ifString            = opTypes       StringType  [BoolType, StringType,  StringType]
    ifPicture           = opTypes       PictureType [BoolType, PictureType, PictureType]

buildinOps      :: [(String, (String, [AttrTree] -> ([AttrTree], Type)))]
buildinOps
    = map (\ (n, FctType resType argTypes)
           -> (n, (n, opTypes resType argTypes))) buildinFcts
