{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- a very simple template substitution system
-- the substitution can run in an arbitrary monad

module Text.SimpleTemplate
where

import           Control.Monad.Reader
import qualified Data.Map.Strict as M
import           Data.Prim.Prelude

-- ----------------------------------------

type TmplName = Text

type TmplPart = Either Text TmplName
type Tmpl     = [TmplPart]

-- ----------------------------------------

-- template variables are referenced by ${name}
--
-- if that does not fit, define another regex and use parseTmpl'
-- for parsing

templateRegex :: Regex
templateRegex =
  parseRegexExt $
  "[$][{][A-Za-z][A-Za-z0-9.]*[}]"

parseTmpl' :: Regex -> (String -> String) -> String -> Tmpl
parseTmpl' r f =
  map ( either
        (Left  . (^. from isoString))
        (Right . (^. from isoString) . f)
      )
  . tokenizeRE' r

parseTmpl :: String -> Tmpl
parseTmpl = parseTmpl' templateRegex (init . drop 2)

-- ----------------------------------------

newtype TmplAct m a = TA {runTmplAct :: TmplName -> TmplEnv m -> m [a]}

newtype TmplEnv m   = TE ( Map TmplName (TmplAct m Text)
                         , Map TmplName Tmpl
                         )

instance (Monad m) => Functor (TmplAct m) where
  fmap f act = TA $ \ n env ->
    map f <$> runTmplAct act n env

instance (Monad m) => Applicative (TmplAct m) where
  pure x = TA $ \ _n _env -> return [x]
  {-# INLINE pure #-}

  f <*> act = TA $ \ n env -> do
    fs <- runTmplAct f   n env
    xs <- runTmplAct act n env
    return [f' x' | f' <- fs, x' <- xs]

instance (Monad m) => Monad (TmplAct m) where
  return = pure
  {-# INLINE return #-}

  act >>= f = TA $ \ n env -> do
    xs <- runTmplAct act n env
    concat <$> mapM (\ x -> runTmplAct (f x) n env) xs

instance (Monad m) => Alternative (TmplAct m) where
  empty = TA $ \ _n _env -> return []
  {-# INLINE empty #-}

  act1 <|> act2 = TA $ \ n env -> do
    xs <- runTmplAct act1 n env
    ys <- runTmplAct act2 n env
    return $ xs ++ ys

instance (Monad m) => MonadPlus (TmplAct m) where
  mzero = empty
  mplus = (<|>)
  {-# INLINE mplus #-}
  {-# INLINE mzero #-}

instance (Monad m) => MonadReader (TmplName, TmplEnv m) (TmplAct m) where
  ask = TA $ \ n env -> return [(n, env)]
  {-# INLINE ask #-}

  local f act = TA $ \ n env ->
    let (n', env') = curry f n env in runTmplAct act n' env'

liftTA :: Monad m => m a -> TmplAct m a
liftTA act = TA $ \ _n _env -> (:[]) <$> act
{-# INLINE liftTA #-}

askTmplName :: Monad m => TmplAct m TmplName
askTmplName = view _1 -- TA $ \ n _env -> return [n]
{-# INLINE askTmplName #-}

askTmplEnv :: Monad m => TmplAct m (TmplEnv m)
askTmplEnv = view _2 -- TA $ \ _n env -> return [env]
{-# INLINE askTmplEnv #-}

localTmplEnv :: (TmplEnv m -> TmplEnv m) -> TmplAct m a -> TmplAct m a
localTmplEnv f act = TA $ \ n env ->
  runTmplAct act n (f env)
{-# INLINE localTmplEnv #-}

ttxt :: (Monad m, IsoText a) => a -> TmplAct m Text
ttxt x = return (x ^. isoText)

xtxt :: (Monad m) => String -> TmplAct m Text
xtxt x = return (x ^. to escHTML . isoText)

-- ----------------------------------------

instance Monoid (TmplEnv m) where
  mempty = TE (M.empty, M.empty)
  {-# INLINE mempty #-}

  (TE te1) `mappend` (TE te2) =
    TE $ (M.union (fst te1) *** M.union (snd te1)) te2


insAct :: TmplName -> TmplAct m Text -> TmplEnv m -> TmplEnv m
insAct n act e =
  e & theTmplAct n .~ Just act
{-# INLINE insAct #-}


insTmpl :: TmplName -> Tmpl -> TmplEnv m -> TmplEnv m
insTmpl n tmpl e =
  e & theTmplEnv n .~ Just tmpl
{-# INLINE insTmpl #-}


insSubTmpl :: Monad m => TmplName -> Tmpl -> TmplEnv m -> TmplEnv m
insSubTmpl n tmpl e =
  e & theTmplEnv n .~ Just tmpl
    & theTmplAct n .~ Just applyTmpl
{-# INLINE insSubTmpl #-}

-- ----------------------------------------

theTEnv :: Iso' (TmplEnv m) ( Map TmplName (TmplAct m Text)
                            , Map TmplName Tmpl
                            )
theTEnv = iso (\ (TE p) -> p) TE
{-# INLINE theTEnv #-}

theTmplAct :: Text -> Lens' (TmplEnv m) (Maybe (TmplAct m Text))
theTmplAct n = theTEnv . _1 . at n
{-# INLINE theTmplAct #-}

theTmplEnv :: Text -> Lens' (TmplEnv m) (Maybe Tmpl)
theTmplEnv n = theTEnv . _2 . at n
{-# INLINE theTmplEnv #-}

-- ----------------------------------------

evalPart :: Monad m => TmplPart -> TmplAct m Text
evalPart (Left  t) = return t
evalPart (Right n) = TA $ \ _n env -> do
  let def = fromMaybe empty $ env ^. theTmplAct "*"
  let act = fromMaybe def   $ env ^. theTmplAct n
  runTmplAct act n env

evalTmpl :: Monad m => Tmpl -> TmplAct m Text
evalTmpl ts = msum $ map evalPart ts
{-# INLINE evalTmpl #-}

-- specialized form of runTmplAct
applyTmpl :: Monad m => TmplAct m Text
applyTmpl = TA $ \ n env ->
  runTmplAct (evalTmpl (env ^. theTmplEnv n . _Just)) n env
{-# INLINE applyTmpl #-}

-- apply a template for a whole sequence of values
-- the template name determines the place where the values
-- are inserted, the action is the insert action for the values,
-- the list contains the values to be inserted
--
-- > insAct "t6" act
-- > where
-- >   act = applySeq "x" (xtxt . show) [1..5]
--
-- installs for the template "t6" an action, which will insert the template
-- for "t6" 5 times, and the template name "x" determines the place, where
-- the values 1..5 of the list will be inserted

applySeq :: Monad m =>
            TmplName -> (a -> TmplAct m Text) -> [a] -> TmplAct m Text
applySeq n act xs = msum $ map lact xs
  where
    lact i = localTmplEnv (insAct n (act i)) $ applyTmpl


applySeqs :: Monad m =>
             [(TmplName, a -> TmplAct m Text)] -> [a] -> TmplAct m Text
applySeqs as xs = msum $ map act xs
  where
    act x = localTmplEnv (ins x) $ applyTmpl
    ins x = foldr (uncurry ins1) id as
      where
        ins1 n ac f = insAct n (ac x) . f

applyCond :: Monad m =>
             Bool -> TmplAct m a -> TmplAct m a
applyCond True act = act
applyCond _    _   = empty

applyNotNull :: (IsEmpty a, Monad m) =>
                a -> TmplAct m Text
applyNotNull x = applyCond (not $ isempty x) applyTmpl

-- ----------------------------------------

escHTML :: String -> String
escHTML = concatMap escChar
  where
    escChar c
      | c > '~' = esc
      | isAlphaNum c = box
      | isSpace c    = box
      | c `elem` xc  = esc
      | c < ' '      = esc
      | otherwise    = box
      where
        xc :: String
        xc = "<>&'\""
        box = [c]
        esc = "&#" ++ show (fromEnum c) ++ ";"

-- ----------------------------------------

{- mini test -}
ee1 :: TmplEnv IO
ee1 =
  mempty
  & insSubTmpl "t1" t1
  & insSubTmpl "t2" t2
  & insSubTmpl "t3" t3
  & insAct "*" (do n <- askTmplName
                   liftTA $ putStrLn $ "unknown template var ignored: " ++ n ^.isoString
                   empty
                )
  & insAct "xxx" (xtxt "yyy")
  & insAct "theo" (xtxt "hello " <|> askTmplName <|> xtxt " bye")
  & insTmpl "t4" t4
  & insAct "t4" (applyTmpl <|> applyTmpl)

  -- a first kind of simple loop wit a single name "no"
  & insTmpl "t5" t5
  & insAct "t5" (applySeq "no" (return . (^. isoString . to escHTML . isoText)) [1..(5::Int)])

  -- template t6 with name "t6"
  -- is inserted 3 times with the values 123, 456 and 789 at name "x""
  -- and the positions 1, 2 and 3 at name "no""
  & insTmpl "t6" t6
  & insAct "t6" (applySeqs [ ("x",  return . (^. _2 . isoString . to escHTML . isoText))
                            , ("no", return . (^. _1 . isoString . to escHTML . isoText))
                            ] $ zip [(1::Int)..] [123, 456, 789::Int]
                 )

condTxt :: Text -> TmplAct IO Text
condTxt t = applyCond (isempty t) (xtxt (t ^. isoString))

t1 :: Tmpl
t1 = parseTmpl "abc${xxx}x${t2}yz${theo}123${t3}456${unknown}789${t5}++${t6}"

t2 :: Tmpl
t2 = parseTmpl "nested ${xxx} template${xxx}"

t3 :: Tmpl
t3 = parseTmpl "two times the nested template ${t4} (t4)"

t4 :: Tmpl
t4 = parseTmpl "((( ${xxx} )))"

t5 :: Tmpl
t5 = parseTmpl "[${no}]"

t6 :: Tmpl
t6 = parseTmpl " ${no}. ${x} "

xxx :: IO [Text]
xxx = runTmplAct applyTmpl "t1" ee1

-- -}
-- ----------------------------------------
