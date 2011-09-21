module Language.Lua.CompileState
    ( runCompile
    , CEnv
    , CGen
    , CErrs
    , CState
    , Compile
    , incrEnvCnt
    , resetEnvCnt
    , theEnvCnt
    , newLoopLevel
    , theLoopLevel
    , emitCode
    , emitErr
    , genNewLabel
    , theNewLabel
    )
where

import Language.Lua.Instr

import Control.Monad.RWS

-- ------------------------------------------------------------
--
-- compile is a reader writer state monad

type Compile = RWS CEnv CGen CState

runCompile :: Compile a -> (ACode, CErrs)
runCompile action
    = let (_v, _cs, res) = runRWS action initCEnv initCState in
      (theCode res, theErrs res)

-- ------------------------------------------------------------
--
-- the env contains an env nesting counter
-- and a list labels and env levels for breaking out of a loop

data CEnv
    = CE { theEnvCnt         :: Int
         , theLoopEnvLevels  :: [(Int, Label)]
         }
      deriving (Show)

initCEnv :: CEnv
initCEnv
    = CE { theEnvCnt        = 0
         , theLoopEnvLevels = []
         }

incrEnvCnt :: CEnv -> CEnv
incrEnvCnt e
    = e { theEnvCnt = theEnvCnt e + 1 }

resetEnvCnt :: CEnv -> CEnv
resetEnvCnt e
    = e { theEnvCnt = 0 }

newLoopLevel :: Label -> CEnv -> CEnv
newLoopLevel l e
    = e { theLoopEnvLevels = (theEnvCnt e, l) : theLoopEnvLevels e }

theLoopLevel :: CEnv -> Maybe (Int, Label)
theLoopLevel env
    = case theLoopEnvLevels env of
        [] -> Nothing
        ((l, lab) : _) -> Just (theEnvCnt env - l, lab)

-- ------------------------------------------------------------
--
-- the writer type: 2 components are collected,
-- the assembler code and the error messages.

data CGen
    = CG { theCode :: ACode
         , theErrs :: CErrs
         }

instance Show CGen where
    show (CG cs es)
              = show cs ++ "\n" ++ show es

instance Monoid CGen where
    mempty
        = CG mempty mempty
    (CG cs1 es1) `mappend` (CG cs2 es2)
        = CG (cs1 `mappend` cs2) (es1 `mappend` es2)

emitErr :: String -> Compile ()
emitErr msg
    = tell $ CG mempty (CErrs [msg])

emitCode :: ACode -> Compile ()
emitCode code
    = tell $ CG code mempty

-- ------------------------------------------------------------

newtype CErrs
    = CErrs [String]

instance Show CErrs where
    show (CErrs es) = "\n" ++ unlines es

instance Monoid CErrs where
    mempty = CErrs []
    mappend (CErrs e1) (CErrs e2) = CErrs $ e1 ++ e2

-- ------------------------------------------------------------
--
-- the state contains a counter for a label generator

data CState
    = CS { _newLab :: Int }
      deriving (Show)

initCState :: CState
initCState
    = CS 1

genNewLabel :: CState -> CState
genNewLabel (CS l)
    = CS (l + 1)

theNewLabel :: CState -> Label
theNewLabel (CS l)
    = Lab l

-- ------------------------------------------------------------
