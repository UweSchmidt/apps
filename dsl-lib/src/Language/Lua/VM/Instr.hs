{-# OPTIONS -fno-warn-orphans #-}

-- ------------------------------------------------------------

-- the simple functions defined on instructions

-- ------------------------------------------------------------

module Language.Lua.VM.Instr
where

import Control.Monad.Trans      ( MonadIO )

import Data.Char            	( toLower )
import Data.List                ( intercalate )

import Language.Lua.VM.Types
import Language.Lua.VM.Value

-- ------------------------------------------------------------

instance Show Code where
    show (Code is) = unlines . map show $ is

-- ------------------------------------------------------------

instance Show MCode where
    show (MCode is) = showMachineCode . Code $ is

-- ------------------------------------------------------------

instance Show Instr where
    show (LoadNum d   ) = fmt1 "load" (show d)
    show (LoadStr s   ) = fmt1 "load" (show s)
    show (LoadBool b  ) = fmt1 "load" (if b then "true" else "false")
    show (LoadNil     ) = fmt1 "load" "nil"
    show (LoadVar n   ) = fmt1 "load" n
    show (LoadField   ) = fmt1 "load" ".[.]"
    show (NewTable    ) = fmt0 "newtab"
    show (NewEnv      ) = fmt0 "newenv"
    show (NewLocal n  ) = fmt1 "newloc" n
    show (DelEnv      ) = fmt0 "delenv"
    show (StoreVar n  ) = fmt1 "store" n
    show (StoreField  ) = fmt1 "store" ".[.]"
    show (AppendField ) = fmt0 "append"

    show (Copy i      ) = fmt1 "copy"  $ show i
    show (Move i      ) = fmt1 "move"  $ show i
    show (BinOp op    ) = fmt0 $ fmtOp $ show op
    show (UnOp op     ) = fmt0 $ fmtOp $ show op
    show (Jump l      ) = fmt1 "jump" (show l)
    show (Label l     ) = fmtL $ show l
    show (Branch b l  ) = fmt1 ("br" ++ if b then "true" else "false") (show l)
    show (Closure l   ) = fmt1 "closure" (show l)
    show (Call        ) = fmt0 "call"
    show (TailCall    ) = fmt0 "tailcall"
    show (Leave       ) = fmt0 "return"
    show (Intr msg    ) = fmt1 "intr" (show msg)

    -- {- the new tuple instructions
    show (MkTup      0) = fmt1 "mktup"     "()"
    show (MkTup      1) = fmt0 "noop"
    show (MkTup      n) = fmt1 "mktup"    (tpl n)
    show (UnTup      0) = fmt0 "pop"
    show (UnTup      1) = fmt0 "fst"
    show (UnTup      n) = fmt1 "untup"    (tpl n)
    show (UnTup'     n) = fmt1 "untup"    (tpl''' n)
    -- -}

    {- the old tuple instructions
    show (MkTuple     ) = fmt0 "mktuple"
    show (UnTuple     ) = fmt0 "untuple"
    show (Take1       ) = fmt0 "take1"
    show (LoadEmpty   ) = fmt1 "load" "()"
    show (Pop         ) = fmt0 "pop"
    -- -}


indent     :: String -> String
indent s   = replicate 8 ' ' ++ s

fill       :: Int -> String -> String
fill n s   = take n (s ++ replicate n ' ')

fmt0       :: String -> String
fmt0 s     = indent s

fmt1       :: String -> String -> String
fmt1 s0 s1 = indent $ fill' 8 s0 ++ s1

fmtL       :: String -> String
fmtL l     = l ++ ":"

fmtOp      :: String -> String
fmtOp      = map toLower

fill'      :: Int -> String -> String
fill' n s
    = fill n' s
    where
      n' = ((length s - 1) `div` n + 1) * n

tpl, tpl''' :: Int -> String
tpl         = tpl' ""
tpl'''      = tpl' "..."

tpl'        :: String -> Int -> String
tpl' s n    = "(" ++ replicate (n - 1) ',' ++ s ++ ")"

-- ------------------------------------------------------------

showMachineCode :: Code -> String
showMachineCode (Code is)
    = unlines . map (uncurry showMachineInstr) $ zip [0..] is

showMachineInstr :: Int -> Instr -> String
showMachineInstr ic instr
    = fmtCnt 4 ++ fmtInstr
    where
      fmtCnt n = (++ replicate (8 - n) ' ') .
                 reverse . take n . reverse .
                 ((replicate n ' ') ++) .
                 show
                 $ ic
      fmtInstr
          = drop 8 $ show instr ++ target instr
          where
            dist                d   = "   --> " ++ show (d + ic)
            target (Jump     (D d)) = dist d
            target (Branch _ (D d)) = dist d
            target (Closure  (D d)) = dist d
            target _                = ""

-- ------------------------------------------------------------

mkCode      :: Instr -> Code
mkCode      = Code . (:[])

mkNoop      :: Code
mkNoop      = Code []

-- ------------------------------------------------------------
--
-- smart constructors

mkBranch    :: Bool -> Label -> Code
mkBranch c l = mkCode $ Branch c (M l)

mkJump,
  mkLabel,
  mkClosure   :: Label -> Code

mkJump      = mkCode . Jump . M
mkLabel     = mkCode . Label
mkClosure   = mkCode . Closure . M

mkLoadNum   :: Double -> Code
mkLoadNum   = mkCode . LoadNum

mkLoadBool  :: Bool -> Code
mkLoadBool  = mkCode . LoadBool

mkBinOp     :: BOp -> Code
mkBinOp     = mkCode . BinOp

mkUnOp      :: UOp -> Code
mkUnOp      = mkCode . UnOp

mkLoadStr,
  mkIntr      :: String -> Code

mkLoadStr   = mkCode . LoadStr
mkIntr      = mkCode . Intr

mkNewEnv,
  mkDelEnv,
  mkCall,
  mkTailCall,
  mkLeave,
  mkLoadField,
  mkStoreField,
  mkAppendField,
  mkLoadNil,
  mkNewTable    :: Code

mkCall      = mkCode Call
mkTailCall  = mkCode TailCall
mkLeave     = mkCode Leave
mkNewEnv    = mkCode NewEnv
mkDelEnv    = mkCode DelEnv
mkLoadField = mkCode LoadField
mkStoreField= mkCode StoreField
mkAppendField= mkCode AppendField
mkLoadNil   = mkCode LoadNil
mkNewTable  = mkCode NewTable

mkDelEnvN,
  mkMove,
  mkCopy   :: Int -> Code

mkDelEnvN n = Code $ replicate n DelEnv
mkMove      = mkCode . Move
mkCopy      = mkCode . Copy

mkNewLocal,
  mkLoadVar,
  mkStoreVar  :: VName -> Code

mkNewLocal  = mkCode . NewLocal
mkLoadVar   = mkCode . LoadVar
mkStoreVar  = mkCode . StoreVar

-- ----------

mkEmptyTup,
  mkPop,
  mkUnTup2,
  mkUnTup1    :: Code

mkTupN, mkUnTupN, mkUnTupN'      :: Int -> Code

-- {-- new tuple instructions

mkEmptyTup  = mkCode $ MkTup 0
mkPop       = mkCode $ UnTup 0
mkUnTup2    = mkCode $ UnTup 2
mkUnTup1    = mkCode $ UnTup 1

mkUnTupN    = mkCode . UnTup
mkUnTupN'   = mkCode . UnTup'

mkTupN 1    = mkNoop
mkTupN n    = mkCode . MkTup $ n 
-- -}

{- -- old tuple instructions

mkEmptyTup  = mkCode $ LoadEmpty
mkPop       = mkCode $ Pop
mkUnTup2    = mkCode $ UnTuple
mkUnTup1    = mkCode $ Take1
mkTupN n    = Code   $ replicate (n - 1) MkTuple
-- -}
-- ------------------------------------------------------------

--
-- trace and dump operations

dumpEvalStack :: (Monad m) => Values -> m String
dumpEvalStack es
    = return $
      unlines $
      [ hl1
      , hl2
      , ""
      ] ++ zipWith fmtCell locs es
    where
      hl1 = "evaluation stack (size: " ++ show (length es) ++ ")"
      hl2 = map (const '=') hl1
      locs = map ( fill 8 . ("top" ++)) $ "" : map show [(-1::Int), -2..]
      fmtCell l v = l ++ ": " ++ show v

-- ------------------------------------------------------------

dumpCallStack :: (Monad m) => [Closure] -> m String
dumpCallStack cs
    = return $
      unlines $
      [ hl1
      , hl2
      , ""
      ] ++ zipWith fmtCl locs cs
    where
      hl1 = "call stack (size: " ++ show (length cs) ++ ")"
      hl2 = map (const '=') hl1
      locs = map ( fill 8 . ("top" ++)) $ "" : map show [(-1::Int), -2..]
      fmtCl l v = l ++ ": ra = " ++ (show . theCA . theCodeAddr) v

-- ------------------------------------------------------------

dumpPC :: (Monad m) => CodeAddress -> m String
dumpPC pc
    = return $
      fill 8 "pc" ++ ": " ++ (show . theCA $ pc)

dumpIntrReg :: (Monad m) => Maybe LuaError -> m String
dumpIntrReg reg
    = return $
      fill 8 "intr" ++ ": " ++
           maybe "-" id reg

-- ------------------------------------------------------------

dumpCurrEnv :: (MonadIO m) => Env -> m String
dumpCurrEnv e
    = do ce <- dumpEnv e
         return $
                fill 8 "env" ++ ": " ++ ce

dumpEnv :: (MonadIO m) => Env -> m String
dumpEnv e
    = do le <- dumpLocEnv e
         ge <- dumpGlob . last . theEnv $ e
         return $ unlines [le, ge]

dumpEnvTable :: (MonadIO m) => Table -> m String
dumpEnvTable et
    = do es <- getEntries et
         return . show . listEntries $ es
{-
dumpLoc :: (MonadIO m) => Env -> m String
dumpLoc env
    = undefined
-}
dumpLocEnv :: (MonadIO m) => Env -> m String
dumpLocEnv
    = dumpTables . init . theEnv		-- the global env is not dumped, only the nested env tables
    where
      indents = (fill 8 "locals" ++ ": ") : repeat (replicate 10 ' ')
      dumpTables ts
          = do ets <- mapM dumpEnvTable ts
               return $ intercalate "\n" $ zipWith (++) indents ets

dumpGlob :: (MonadIO m) => Table -> m String
dumpGlob gt
    = do ge <- dumpTable gt
         return $ unlines $ zipWith (++) ((fill 8 "globals" ++ ": ") : repeat (replicate 10 ' ')) ge

dumpTable :: (MonadIO m) => Table -> m [String]
dumpTable et
    = do es <- getEntries et
         return (map (uncurry dumpPair) . listEntries $ es)
    where
      dumpPair k v
          = fill' 8 (show k) ++ ":-> " ++ show v

-- ------------------------------------------------------------

dumpLuaState :: (MonadIO m) => LuaState -> m String
dumpLuaState s
    = do pc <- dumpPC        . thePC        $ s
         ir <- dumpIntrReg   . theIntrReg   $ s
         ce <- dumpEnv       . theCurrEnv   $ s
         es <- dumpEvalStack . theEvalStack $ s
         cs <- dumpCallStack . theCallStack $ s
         return $ unlines [pc, ir, ce, es, cs]

-- ------------------------------------------------------------

dumpTOS :: String -> Value -> String
dumpTOS msg v
    = fmt1 ("+      " ++ msg) $ show v

-- ------------------------------------------------------------

