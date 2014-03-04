{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PPL.CodeGeneration where

import PPL.AbstractSyntax
import PPL.Instructions
import PPL.GlobalState

import Data.Maybe

-- -------------------------------------------------------------------
-- label generation

initLabel       :: GS ()
initLabel       = updateState "newlabel" (LabCnt 0)

newLabel        :: GS Label
newLabel
    = do
      LabCnt l <- lookupState "newlabel"
      updateState "newlabel" (LabCnt (l+1))
      return ("l" ++ show l)

-- -------------------------------------------------------------------
-- variable allocation

setAllocator    :: Alloc -> GS ()
setAllocator a
    = updateState "alloc" (Allocator a)

getAllocator    :: GS Alloc
getAllocator
    = do
      Allocator a <- lookupState "alloc"
      return a

initAddr        :: GS ()
initAddr
    = do
      updateState "maxaddr" (AddrCnt 0)
      setAddrCnt 0

newAddr         :: GS Int
newAddr
    = do
      a <- getAddrCnt
      setAddrCnt (a + 1)
      return a

setAddrCnt              :: Int -> GS ()
setAddrCnt a
    = do
      updateState "newaddr" (AddrCnt a)
      m <- getDataSegLen
      updateState "maxaddr" (AddrCnt (a `max` m))

getDataSegLen           :: GS Int
getDataSegLen
    = do
      AddrCnt a <- lookupState "maxaddr"
      return a

getAddrCnt              :: GS Int
getAddrCnt
    = do
      AddrCnt a <- lookupState "newaddr"
      return a

initAddrList    :: GS ()
initAddrList
    = do
      setAddrList []

getAddrList     :: GS AddrList
getAddrList
    = do
      AddrList al <- lookupState "addrlist"
      return al

setAddrList     :: AddrList -> GS ()
setAddrList al
    = updateState "addrlist" (AddrList al)

getAddr         :: String -> GS Address
getAddr var
    = do
      al <- getAddrList
      return (maybe (AbsA 0) id (lookup var al))

allocVar        :: String -> GS ()
allocVar id'
    = do
      al    <- getAddrList
      addr  <- allocCell
      setAddrList ((id', addr):al)

allocCell       :: GS Address
allocCell
    = do
      a     <- newAddr
      alloc <- getAllocator
      return (alloc a)

-- -------------------------------------------------------------------

codegeneration  :: AttrTree -> Executable
codegeneration  = snd . compProg

compProg        :: AttrTree -> (State, Executable)
compProg p
    = let
      GS compile = compProg' p
      in
      compile initialState

compProg'       :: AttrTree -> GS Executable
compProg' p
    = do
      initLabel
      initAddr
      initAddrList
      is <- compProg'' p
      ds <- getDataSegLen
      return (is, ds)

compProg''      :: AttrTree -> GS Code
compProg'' (Opr "sequence" (body : decll), _)
    = do
                                -- allocate global variables
      initAddr
      setAllocator AbsA
      sequence_ (map compGlobDecl (filter isGlobDecl decll))

                                -- code for global variable init
      initCode  <- sequence (map compExpr (filter isGlobInit decll))
                                -- code for main program
      bodyCode  <- compExpr body
      len       <- getDataSegLen

                                -- code for functions
      fctCode   <- sequence (map compExpr (filter isGlobFct  decll))

                                -- restore global data segement length
      initAddr
      setAddrCnt len
      setAllocator AbsA

      return ( concat initCode
               ++
               bodyCode
               ++
               [ Compute OPterminate ]
               ++
               concat fctCode
             )


compGlobDecl    :: AttrTree -> GS ()
compGlobDecl (Opr _ [(Ident id', _t)], _)
    = do
      allocVar id'
      return ()

isGlobDecl      :: AttrTree -> Bool
isGlobInit      :: AttrTree -> Bool
isGlobFct       :: AttrTree -> Bool

isGlobDecl (Opr "decl" _, _)    = True
isGlobDecl _                    = False

isGlobInit (Opr ":=" _, _)      = True
isGlobInit _                    = False

isGlobFct (Opr "fctdecl" _, _)  = True
isGlobFct _                     = False

-- -------------------------------------------------------------------
-- compile statements

compExpr        :: AttrTree -> GS Code

compExpr (Opr "fctdecl" ((Ident id', _) : body : fpl), rt)
    = do
                                -- initialize counter for local data segment
      initAddr
      setAllocator LocA
                                -- allocate cell for return address
      retAddr   <- allocCell
                                -- allocate parameter and gen code
                                -- to initialize them
      paramCode <- sequence (map compStoreParam (reverse fpl))

                                -- compile function body
      bodyCode  <- compExpr body
                                -- get size of local data segment
      len       <- getDataSegLen
                                -- combine entry code
                                -- parameter saving code
                                -- function body code
                                -- and exit code
      return ( [ Label ("_" ++ id')

               , Entry len      -- allocate local data segment
                                -- save return address
               , Store retAddr
                                -- mark start of fct body
               , Label ("s_" ++ id')
               ]
               ++
               concat paramCode
               ++
               bodyCode
               ++
               ( if rt == VoidType
                 then [ LoadU ]
                 else []
               )
               ++
                                -- mark end of body
               [ Label ("e_" ++ id')
                                -- load return address
               , Load retAddr
                                -- deallocate local data segement
               , Exit
                                -- jump back to calling point
               , PopJ
               ]
             )


compExpr (Opr "begin" [e], _)
    = do
      addrCnt   <- getAddrCnt
      addrList  <- getAddrList
      code      <- compExpr e
      setAddrList addrList
      setAddrCnt addrCnt
      return code

compExpr (Opr "decl" [(Ident id', _t)], _)
    = do
      allocVar id'
      return []

compExpr (Opr "sequence" sl, _)
    = do
      sl1 <- sequence (map compExpr sl)
      return (concat sl1)

compExpr (Opr "while" [cond, body], _)
    = do
      l1 <- newLabel
      l2 <- newLabel
      codeBody <- compExpr body
      codeCond <- compBranch l2 True cond
      return ( [ Jump (Symb l1) ]
               ++
               [ Label l2 ]
               ++
               codeBody
               ++
               [ Label l1 ]
               ++
               codeCond
             )

compExpr (Opr "repeat" [body, cond], _)
    = do
      l1 <- newLabel
      codeBody <- compExpr body
      codeCond <- compBranch l1 False cond
      return ( [ Label l1 ]
               ++
               codeBody
               ++
               codeCond
             )

compExpr (Opr "if" [cond, thenp, elsep], _)
    = do
      l1        <- newLabel
      l2        <- newLabel
      codeCond  <- compBranch l1 False cond
      codeThenp <- compExpr thenp
      codeElsep <- compExpr elsep
      if length codeElsep == 0
         then
         return ( codeCond
                  ++ codeThenp
                  ++ [ Label l1 ]
                )
         else
         return ( codeCond
                  ++ codeThenp
                  ++ [ Jump (Symb l2) ]
                  ++ [ Label l1 ]
                  ++ codeElsep
                  ++ [ Label l2 ]
                )

compExpr (Opr ":=" asl, _)
    = do
      let l     = length asl `div` 2
      let vl    = take l asl
      let el    = drop l asl
      codeRHS   <- sequence (map compExpr el)
      codeLHS   <- sequence (map compStore (reverse vl))
      return (concat codeRHS ++ concat codeLHS)

compExpr (Opr "do" [e], _)
    = do
      c <- compExpr e
      return (c ++ [Pop])

-- -------------------------------------------------------------------
-- transform non strict boolean operators

                -- a and b -> if a then b else false

compExpr (Opr "and" [e1,e2], t)
    = compExpr (Opr "if" [e1, e2, boolConstFalse], t)

                -- a or b  -> if a then true else b

compExpr (Opr "or" [e1,e2], t)
    = compExpr (Opr "if" [e1, boolConstTrue, e2], t)

                -- a => b -> if a then b else true

compExpr (Opr "impl" [e1,e2], t)
    = compExpr (Opr "if" [e1, e2, boolConstTrue], t)

                -- eliminate xor ops

compExpr (Opr "xor" el, t)
    = compExpr (Opr "not" el', t)
      where
      el' = [(Opr "equiv" el, t)]

                -- map equivalence to integer =
compExpr (Opr "equiv" el, _t)
    = compExpr (Opr "eqi" el, IntType)

                -- not a -> 1 - a

compExpr (Opr "not" [e], _t)
    = compExpr (Opr "subi" [intConst1, e], IntType)


compExpr (Opr "b2s" [e], BoolType)
    = compExpr (Opr "if" [ e
                         , stringConstTrue
                         , stringConstFalse
                         ]
               , StringType
               )

-- -------------------------------------------------------------------
                -- unary + elimination
compExpr (Opr "ident" [e], _)
    = compExpr e

                -- unary - partial evaluation

compExpr (Opr "negi" [(IntVal i, _)], t)
    = compExpr (IntVal (0 - i), t)

compExpr (Opr "negf" [(FloatVal f, _)], t)
    = compExpr (FloatVal (0.0 - f), t)

                -- -a -> 0 - a

compExpr (Opr "negi" [e], t)
    = compExpr (Opr "subi" [intConst0, e], t)

compExpr (Opr "negf" [e], t)
    = compExpr (Opr "subf" [floatConst0, e], t)

                -- eliminate /= ops

compExpr (Opr op el, t)
    | isNeOp op
        = compExpr (Opr "not" el', t)
          where
          el' = [(Opr (getComplOp op) el, t)]

                -- use arg types for rel ops translation

compExpr (Opr op el, BoolType)
    | isRelOp op
        = compExpr (Opr op el, t1)
          where
          (_,t1):_ = el


                -- "a < b" -> "b > a", "a <= b" -> "b >= a"

compExpr (Opr op [e1,e2], t)
    | isSymOp op
        = compExpr (Opr (symOp op) [e2,e1], t)


-- -------------------------------------------------------------------
-- transform list ops

compExpr (Opr "headl" [e], t)
    = compExpr (Opr "indexl" [e, (IntVal 0, IntType)], t)

-- -------------------------------------------------------------------
-- compile constants and variables

compExpr (BoolVal b, _)
    = return [ LoadI (fromEnum b)]

compExpr (IntVal i, _)
    = return [ LoadI i]

compExpr (FloatVal f, _)
    = return [ LoadF f]

compExpr (StringVal s, _)
    = return [ LoadS s]

compExpr (EmptyList , _)
    = return [ LoadEL ]

compExpr (UndefVal , _)
    = return [ LoadU ]

compExpr (Ident id', _)
    = do
      a <- getAddr id'
      return [ Load a ]

-- -------------------------------------------------------------------

-- compile build in operations

compExpr (Opr op al, _t)
    | op `elem` svcFcts
    = do
      codeArgl <- sequence (map compExpr al)
      return ( concat codeArgl
               ++
               [SysCall op]
             )

compExpr (Opr "definedfct" ((StringVal fn, _):al), _t)
    = do
      codeArgl <- sequence (map compExpr al)
      return ( concat codeArgl
               ++
               [PushJ (Symb ("_" ++ fn))]
             )

compExpr (Opr op al, _t)
    = do
      codeArgl <- sequence (map compExpr al)
      return ( concat codeArgl
               ++
               [ Compute (fromJust (lookup op opr2Opcode)) ]
             )

-- -------------------------------------------------------------------

-- default: error or not yet implemented

compExpr e
    = return [IllegalInstr (show e)]

-- -------------------------------------------------------------------

undefValue      :: AttrTree
intConst0       :: AttrTree
intConst1       :: AttrTree
floatConst0     :: AttrTree
boolConstTrue   :: AttrTree
boolConstFalse  :: AttrTree
stringConstTrue :: AttrTree
stringConstFalse:: AttrTree
whiteValue      :: AttrTree
blackValue      :: AttrTree

undefValue      = (UndefVal, UnknownType)
intConst0       = (IntVal 0, IntType)
intConst1       = (IntVal 1, IntType)
floatConst0     = (FloatVal 0.0, FloatType)
boolConstTrue   = (BoolVal True, BoolType)
boolConstFalse  = (BoolVal False, BoolType)
stringConstTrue = (StringVal "true", StringType)
stringConstFalse= (StringVal "false", StringType)
whiteValue      = (FloatVal 1.0, FloatType)
blackValue      = (FloatVal 0.0, FloatType)

appendNl        :: AttrTree -> AttrTree
appendNl e
    = (Opr "+" [e, (StringVal "\n", StringType)], StringType)

-- -------------------------------------------------------------------

-- opcode tables

complOps        :: [(String, String)]
complOps
    = [ ("nei", "eqi")
      , ("nef", "eqf")
      , ("nes", "eqs")
      ]

symOps          :: [(String, String)]
symOps
    = [ ("lti", "gti")
      , ("ltf", "gtf")
      , ("lei", "gei")
      , ("lef", "gef")
      ]

isOp            :: [(String, String)] -> String -> Bool
isOp ops op
    = op `elem` (map fst ops)

getOp           :: [(String, String)] -> String -> String
getOp table op
    = let
      Just opcode = lookup op table
      in opcode

isNeOp          :: String -> Bool
isSymOp         :: String -> Bool

isNeOp          = isOp complOps
isSymOp         = isOp symOps

getComplOp      :: String -> String
symOp           :: String -> String

getComplOp      = getOp complOps
symOp           = getOp symOps

isRelOp         :: String -> Bool
isRelOp op
    = op `elem` ["eqi", "eqf", "eqs"
                ,"gei", "gef", "ges"
                ,"gti", "gtf", "gts"
                ]

svcFcts         :: [String]
svcFcts
    = [ "write"
      , "writeln"
      , "getArgs"
      , "dump"
      , "load"
      , "store"
      ]

-- -------------------------------------------------------------------
-- compile store

compStore       :: AttrTree -> GS Code
compStore (Ident id', _)
    = do
      a <- getAddr id'
      return [ Store a ]

compStoreParam  :: AttrTree -> GS Code
compStoreParam (Opr "decl" [var@(Ident id', _t)], _)
    = do
      allocVar id'
      compStore var

-- -------------------------------------------------------------------
-- compile branches

compBranch      :: Label -> Bool -> AttrTree -> GS Code

compBranch lab cond (BoolVal b, _)
    | cond == b
        = return [Jump (Symb lab)]
    | otherwise
        = return []

compBranch lab cond (Opr "not" [e1], _)
    = compBranch lab (not cond) e1

compBranch lab cond (Opr "and" [e1,e2], _)
    = if cond
         then
         do
         lab1   <- newLabel
         cond1  <- compBranch lab1 (not cond) e1
         cond2  <- compBranch lab cond e2
         return ( cond1
                  ++
                  cond2
                  ++
                  [ Label lab1 ]
                )
         else
         do
         cond1  <- compBranch lab cond e1
         cond2  <- compBranch lab cond e2
         return ( cond1
                  ++
                  cond2
                )

                -- apply de Morgan

compBranch lab cond (Opr "or" [e1,e2], _)
    = compBranch lab (not cond) (Opr "and" el, BoolType)
      where
      el = [ (Opr "not" [e1], BoolType)
           , (Opr "not" [e2], BoolType)
           ]

                -- apply de Morgan

compBranch lab cond (Opr "impl" [e1,e2], _)
    = compBranch lab (not cond) (Opr "and" el, BoolType)
      where
      el = [ e1
           , (Opr "not" [e2], BoolType)
           ]

                -- eliminate xor ops

compBranch lab cond (Opr "xor" el, _)
    = compBranch lab (not cond) (Opr "equiv" el, BoolType)

                -- eliminate /= ops

compBranch lab cond (Opr op el, _)
    | isNeOp op
        = compBranch lab (not cond) (Opr (getComplOp op) el, BoolType)

compBranch lab cond expr
    = do
      is <- compExpr expr
      return ( is
               ++
               [Branch cond (Symb lab)]
             )

-- --------------------
--
-- mapping of node labels to ppl opcodes

opr2Opcode :: [(String, Opcode)]
opr2Opcode
    = [ ("abort",       OPabort)
      , ("above",       OPabove)
      , ("addf",        OPaddf)
      , ("addi",        OPaddi)
      , ("appendl",     OPappendl)
      , ("bitmap",      OPbitmap)
      , ("black",       OPblack)
      , ("blackAndWhite",       OPblackAndWhite)
      , ("concatHorizontal",    OPconcatHorizontal)
      , ("concatVertical",      OPconcatVertical)
      , ("concl",       OPconcl)
      , ("concs",       OPconcs)
      , ("cut",         OPcut)
      , ("diff",        OPdiff)
      , ("divf",        OPdivf)
      , ("divi",        OPdivi)
      , ("eqf",         OPeqf)
      , ("eqi",         OPeqi)
      , ("exit",        OPterminate)
      , ("f2s",         OPf2s)
      , ("flipDiagonal",        OPflipDiagonal)
      , ("flipHorizontal",      OPflipHorizontal)
      , ("flipVertical",        OPflipVertical)
      , ("gamma",       OPgamma)
      , ("gef",         OPgef)
      , ("gei",         OPgei)
      , ("grey",        OPgrey)
      , ("gtf",         OPgtf)
      , ("gti",         OPgti)
      , ("height",      OPheight)
      , ("i2f",         OPi2f)
      , ("i2s",         OPi2s)
      , ("indexl",      OPindexl)
      , ("inverseDiff", OPinverseDiff)
      , ("inverseMean", OPinverseMean)
      , ("invert",      OPinvert)
      , ("isemptyl",    OPisemptyl)
      , ("lengthl",     OPlengthl)
      , ("maxf",        OPmaxf)
      , ("maxi",        OPmaxi)
      , ("maxp",        OPmaxp)
      , ("mean",        OPmean)
      , ("mergeHorizontal",     OPmergeHorizontal)
      , ("mergeVertical",       OPmergeVertical)
      , ("minf",        OPminf)
      , ("mini",        OPmini)
      , ("minp",        OPminp)
      , ("modi",        OPmodi)
      , ("mulf",        OPmulf)
      , ("muli",        OPmuli)
      , ("mulp",        OPmulp)
      , ("partitionHorizontal", OPpartitionHorizontal)
      , ("partitionVertical",   OPpartitionVertical)
      , ("paste",       OPpaste)
      , ("consl",       OPconsl)
      , ("reduceColor", OPreduceColor)
      , ("replicate",   OPreplicate)
      , ("resize",      OPresize)
      , ("rotate",      OProtate)
      , ("round",       OPround)
      , ("scale",       OPscale)
      , ("shift",       OPshift)
      , ("shrink",      OPshrink)
      , ("sideBySide",  OPsideBySide)
      , ("splitHorizontal",     OPsplitHorizontal)
      , ("splitVertical",       OPsplitVertical)
      , ("subf",        OPsubf)
      , ("subi",        OPsubi)
      , ("taill",       OPtaill)
      , ("trunc",       OPtrunc)
      , ("white",       OPwhite)
      , ("width",       OPwidth)
      ]
