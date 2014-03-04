{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-missing-signatures #-}

module PPL.OPCode
    ( ops
    , svcs
    ) where

import PPL.Instructions
import PPL.MachineArchitecture
import PPL.MicroCode
import PPL.Picture
import PPL.Error
import PPL.ShowMS

import System.Environment

-- -------------------------------------------------------------------

-- read the arguments from evaluation stack
-- and check their types

getInstrArgs :: Monad m => [m b] -> m [b]
getInstrArgs    al = do
             vl <- sequence (reverse al)
             return (reverse vl)

getNoArg        :: MST [MV]
getNoArg        = getInstrArgs []
getFloat        = getInstrArgs [popFloat]
getInt          = getInstrArgs [popInt]
getList         = getInstrArgs [popList]
getPic          = getInstrArgs [popPic]
getStr          = getInstrArgs [popString]
getFloat2       = getInstrArgs [popFloat,       popFloat]
getInt2         = getInstrArgs [popInt,         popInt]
getListAny      = getInstrArgs [popList,        popMV]
getListInt      = getInstrArgs [popList,        popInt]
getList2        = getInstrArgs [popList,        popList]
getPicFloat     = getInstrArgs [popPic,         popFloat]
getPicInt       = getInstrArgs [popPic,         popInt]
getPicStr       = getInstrArgs [popPic,         popString]
getPic2         = getInstrArgs [popPic,         popPic]
getStr2         = getInstrArgs [popString,      popString]
getFloatInt2    = getInstrArgs [popFloat,       popInt, popInt]
getPicInt2      = getInstrArgs [popPic,         popInt, popInt]
getPic2Int2     = getInstrArgs [popPic,         popPic, popInt, popInt]
getPicInt4      = getInstrArgs [popPic,         popInt, popInt, popInt, popInt]

-- -------------------------------------------------------------------

-- the table of opcode functions consists of
-- 2 functions, the first one for reading and checking
-- the arguments, the second for doing the real operation

ops             :: [(Opcode, (MST [MV], [MV] -> MST MV))]

ops = map ( \ (op, (get, eval)) -> (op, (get, liftCompute eval)))
      [ (OPaddi,        (getInt2,       evalAddi))
      , (OPsubi,        (getInt2,       evalSubi))
      , (OPmuli,        (getInt2,       evalMuli))
      , (OPdivi,        (getInt2,       evalDivi))
      , (OPmodi,        (getInt2,       evalModi))
      , (OPmaxi,        (getInt2,       evalMaxi))
      , (OPmini,        (getInt2,       evalMini))
      , (OPeqi,         (getInt2,       evalEqi))
      , (OPgei,         (getInt2,       evalGei))
      , (OPgti,         (getInt2,       evalGti))

      , (OPincri,       (getInt,        evalIncri))
      , (OPdecri,       (getInt,        evalDecri))

      , (OPaddf,        (getFloat2,     evalAddf))
      , (OPsubf,        (getFloat2,     evalSubf))
      , (OPmulf,        (getFloat2,     evalMulf))
      , (OPdivf,        (getFloat2,     evalDivf))
      , (OPmaxf,        (getFloat2,     evalMaxf))
      , (OPminf,        (getFloat2,     evalMinf))
      , (OPeqf,         (getFloat2,     evalEqf))
      , (OPgef,         (getFloat2,     evalGef))
      , (OPgtf,         (getFloat2,     evalGtf))

      , (OPi2s,         (getInt,        evalI2s))
      , (OPf2s,         (getFloat,      evalF2s))
      , (OPi2f,         (getInt,        evalI2f))
      , (OPtrunc,       (getFloat,      evalTrunc))
      , (OPround,       (getFloat,      evalRound))

      , (OPconcs,       (getStr2,       evalConcs))

      , (OPisemptyl,    (getList,       evalIsEmpty))
      , (OPlengthl,     (getList,       evalLength))
      , (OPtaill,       (getList,       evalTail))
      , (OPconcl,       (getList2,      evalConcl))
      , (OPconsl,       (getListAny,    evalCons))
      , (OPappendl,     (getListAny,    evalAppend))
      , (OPindexl,      (getListInt,    evalIndex))

      , (OPwidth,       (getPic,        evalWidth))
      , (OPheight,      (getPic,        evalHeight))
      , (OPblack,       (getInt2,       evalBlack))
      , (OPwhite,       (getInt2,       evalWhite))
      , (OPgrey,        (getFloatInt2,  evalGrey))

      , (OPgamma,               (getPicFloat,   evalGamma))
      , (OPinvert,              (getPic,        evalPic1 invertPic))
      , (OPbitmap,              (getPic,        evalPic1 bitmapPic))
      , (OPblackAndWhite,       (getPic,        evalPic1 blackAndWhitePic))
      , (OPreduceColor,         (getPicInt,     evalReduceColor))
      , (OPflipHorizontal,      (getPic,        evalPic1 flipHMx))
      , (OPflipVertical,        (getPic,        evalPic1 flipVMx))
      , (OPflipDiagonal,        (getPic,        evalPic1 flipDMx))
      , (OProtate,              (getPic,        evalPic1 rotateMx))
      , (OPshift,               (getPicInt2,    evalPicII shiftMx))
      , (OPcut,                 (getPicInt4,    evalCut))
      , (OPpaste,               (getPic2Int2,   evalPaste))
      , (OPscale,               (getPicInt2,    evalPicII scaleMx))
      , (OPshrink,              (getPicInt2,    evalPicII shrinkMx))
      , (OPreplicate,           (getPicInt2,    evalPicII replicateMx))
      , (OPresize,              (getPicInt2,    evalPicII resizePic))
      , (OPsideBySide,          (getPic2,       evalPic2 sideBySideMx))
      , (OPabove,               (getPic2,       evalPic2 aboveMx))

      , (OPpartitionHorizontal, (getPicInt,     evalPartition partHMx))
      , (OPpartitionVertical,   (getPicInt,     evalPartition partVMx))
      , (OPsplitHorizontal,     (getPicInt,     evalPartition splitHMx))
      , (OPsplitVertical,       (getPicInt,     evalPartition splitVMx))
      , (OPmergeHorizontal,     (getPic2,       evalPic2 mergeHMx))
      , (OPmergeVertical,       (getPic2,       evalPic2 mergeVMx))
      , (OPconcatHorizontal,    (getList,       evalPicList concatHMx))
      , (OPconcatVertical,      (getList,       evalPicList concatVMx))

      , (OPmean,                (getPic2,       evalPic2 meanPic))
      , (OPdiff,                (getPic2,       evalPic2 diffPic))
      , (OPinverseMean,         (getPic2,       evalPic2 invMeanPic))
      , (OPinverseDiff,         (getPic2,       evalPic2 invDiffPic))
      , (OPmulp,                (getPic2,       evalPic2 mulPic))
      , (OPmaxp,                (getPic2,       evalPic2 maxPic))
      , (OPminp,                (getPic2,       evalPic2 minPic))

      , (OPterminate,           (getNoArg,      (\_ -> Error programTerminated)))
      , (OPabort,               (getStr,        (\ [VString s] -> Error (programAborted ++ ": " ++ s))))
      ]

-- -------------------------------------------------------------------

-- the table of SVC calls and their functions
-- 2 functions are needed, the get...Arg for
-- reading the function arguments from the evaluation stack
-- and the eval??? function for doing the read work
-- these functions are different from normal operations,
-- because they may do some IO

svcs    :: [(String, (MST [MV], [MV] -> MST MV))]

svcs = map ( \ (op, (get, eval)) -> (op, (get, liftSvc eval)))
       [ ("write",      (getStr,        evalWrite putStr))
       , ("writeln",    (getStr,        evalWrite putStrLn))
       , ("getArgs",    (getNoArg,      evalGetArgs))
       , ("load",       (getStr,        evalLoadPic))
       , ("store",      (getPicStr,     evalStorePic))
       , ("abort",      (getNoArg,      evalAbort))
       , ("exit",       (getNoArg,      evalExit))
       ] ++
       [ ("dump",       (getNoArg,      evalDumpMS))
       ]

-- -------------------------------------------------------------------

evalWrite put [VString s]
    = put s >> return (OK VUndef)

evalGetArgs []
    = do
      argl <- getArgs
      return (OK (VList (map VString (drop 2 argl))))

evalAbort []
    = return (Error programAborted)

evalExit []
    = return (Error programTerminated)

evalLoadPic [VString s]
    = do
      c <- readPictureFile s
      return (OK (VPic c))

evalStorePic [VPic p, VString s]
    = do
      writePictureFile s p
      return (OK VUndef)

evalDumpMS []
    = do
      trc showMS
      return VUndef

-- -------------------------------------------------------------------

-- integer arithmetic

evalAddi [VInt i1, VInt i2]     = OK (VInt (i1 + i2))
evalSubi [VInt i1, VInt i2]     = OK (VInt (i1 - i2))
evalMuli [VInt i1, VInt i2]     = OK (VInt (i1 * i2))

evalDivi [_      , VInt 0 ]     = Error "integer division by zero"
evalDivi [VInt i1, VInt i2]     = OK (VInt (i1 `div` i2))

evalModi [_      , VInt 0 ]     = Error "remainder by zero"
evalModi [VInt i1, VInt i2]     = OK (VInt (i1 `mod` i2))

evalMaxi [VInt i1, VInt i2]     = OK (VInt (i1 `max` i2))
evalMini [VInt i1, VInt i2]     = OK (VInt (i1 `min` i2))

evalEqi  [VInt i1, VInt i2]     = OK (VInt (fromEnum (i1 == i2)))
evalGei  [VInt i1, VInt i2]     = OK (VInt (fromEnum (i1 >= i2)))
evalGti  [VInt i1, VInt i2]     = OK (VInt (fromEnum (i1 >  i2)))

evalIncri [VInt i1]             = OK (VInt (i1 + 1))
evalDecri [VInt i1]             = OK (VInt (i1 - 1))

-- floating arithmetic

evalAddf [VFloat f1, VFloat f2] = OK (VFloat (f1 + f2))
evalSubf [VFloat f1, VFloat f2] = OK (VFloat (f1 - f2))
evalMulf [VFloat f1, VFloat f2] = OK (VFloat (f1 * f2))

evalDivf [_       , VFloat 0.0] = Error "floating division by zero"
evalDivf [VFloat f1, VFloat f2] = OK (VFloat (f1 / f2))

evalMaxf [VFloat f1, VFloat f2] = OK (VFloat (f1 `max` f2))
evalMinf [VFloat f1, VFloat f2] = OK (VFloat (f1 `min` f2))

evalEqf  [VFloat f1, VFloat f2] = OK (VInt (fromEnum (f1 == f2)))
evalGef  [VFloat f1, VFloat f2] = OK (VInt (fromEnum (f1 >= f2)))
evalGtf  [VFloat f1, VFloat f2] = OK (VInt (fromEnum (f1 >  f2)))

-- conversions

evalI2s  [VInt i]               = OK (VString (show i))
evalF2s  [VFloat f]             = OK (VString (show f))
evalI2f  [VInt i]               = OK (VFloat ((fromInteger . toInteger) i))

evalTrunc [VFloat f]            = OK (VInt (truncate f))
evalRound [VFloat f]            = OK (VInt (round f))

-- string operations

evalConcs [VString s1, VString s2] = OK (VString (s1 ++ s2))

-- list operations

evalIsEmpty [VList l]           = OK (VInt (fromEnum (null l)))
evalLength  [VList l]           = OK (VInt (length l))

evalTail    [VList []]          = Error "tail of empty list"
evalTail    [VList (_:l)]       = OK (VList l)

evalConcl   [VList l1, VList l2]
                                = OK (VList (l1 ++ l2))
evalCons    [VList l1, x2]      = OK (VList (x2:l1))
evalAppend  [VList l1, x2]      = OK (VList (l1 ++ [x2]))
evalIndex   [VList l1, VInt ix]
    | ix < 0
      = Error ( "negative list index: "
                ++ show ix
              )
    | ix >= length l1
        = Error ( "index out of bounds: ix = "
                  ++ show ix
                  ++ ", but list length is "
                  ++ show (length l1)
                )
    | otherwise
        = OK (l1 !! ix)

-- picture operations

evalWidth  [VPic p]     = OK (VInt (widthMx  p))
evalHeight [VPic p]     = OK (VInt (heightMx p))

evalGrey [VFloat l, VInt w, VInt h]
    = OK (VPic (greyPic l w h))

evalWhite al            = evalGrey (VFloat 1.0 : al)
evalBlack al            = evalGrey (VFloat 0.0 : al)

evalGamma [VPic p, VFloat f]
    = OK (VPic (gammaPic f p))

evalPic1 f [VPic p]     = OK (VPic (f p))

evalPicII f [VPic p, VInt w, VInt h]
    = OK (VPic (f w h p))

evalCut [VPic p, VInt x, VInt y, VInt w, VInt h]
    = OK (VPic (cutMx x y w h p))

evalPaste [VPic p1, VPic p2, VInt x, VInt y]
    = OK (VPic (pasteMx x y p2 p1))

evalPic2 f [VPic p1, VPic p2]
    = OK (VPic (f p1 p2))

evalReduceColor [VPic p, VInt i]
    = OK (VPic (reduceColorPic i p))

evalPartition f [VPic p1, VInt n]
    | n <= 0
      = Error ( "# of partitions must be > 0: "
                ++ show n
              )
    | otherwise
      = OK (VList (map (\ p -> VPic p) res))
        where
        res = f n p1

evalPicList f [VList ps]
    = OK (VPic (f (map ( \ (VPic p) -> p ) ps)))

-- -------------------------------------------------------------------
