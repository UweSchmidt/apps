{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- a very simple template substitution system
-- the substitution can run in an arbitrary monad

module Text.SimpleTemplate
where

import qualified Data.Map.Strict as M
import           Data.Prim.Prelude
import Control.Monad.Reader

-- ----------------------------------------

type Template = [Either Text Text]

newtype EnvTmpl m = EnvTmpl ( Map Text (TmplAct m)
                    , Map Text Template
                    )

type TmplAct m = Text -> EnvTmpl m -> m [Text]

-- ----------------------------------------

type TmplName = Text

type TmplPart = Either Text TmplName
type Tmpl     = [TmplPart]

newtype TmplAct' m a = TA {runTmplAct :: TmplName -> TmplEnv m -> m [a]}

newtype TmplEnv m = TE ( Map TmplName (TmplAct' m Text)
                       , Map TmplName Tmpl
                       )

instance (Monad m) => Functor (TmplAct' m) where
  fmap f act = TA $ \ n env ->
    map f <$> runTmplAct act n env

instance (Monad m) => Applicative (TmplAct' m) where
  pure x = TA $ \ _n _env -> return [x]

  f <*> act = TA $ \ n env -> do
    fs <- runTmplAct f   n env
    xs <- runTmplAct act n env
    return [f' x' | f' <- fs, x' <- xs]

instance (Monad m) => Monad (TmplAct' m) where
  return = pure

  act >>= f = TA $ \ n env -> do
    xs <- runTmplAct act n env
    concat <$> mapM (\ x -> runTmplAct (f x) n env) xs

instance (Monad m) => Alternative (TmplAct' m) where
  empty = TA $ \ _n _env -> return []
  act1 <|> act2 = TA $ \ n env -> do
    xs <- runTmplAct act1 n env
    ys <- runTmplAct act2 n env
    return $ xs ++ ys

instance (Monad m) => MonadPlus (TmplAct' m) where
  mzero = empty
  mplus = (<|>)

instance (Monad m) => MonadReader (TmplName, TmplEnv m) (TmplAct' m) where
  ask = TA $ \ n env -> return [(n, env)]

  local f act = TA $ \ n env ->
    let (n', env') = curry f n env in runTmplAct act n' env'

liftTA :: Monad m => m a -> TmplAct' m a
liftTA act = TA $ \ _n _env -> (:[]) <$> act

askTmplName :: Monad m => TmplAct' m TmplName
askTmplName = view _1 -- TA $ \ n _env -> return [n]

askTmplEnv :: Monad m => TmplAct' m (TmplEnv m)
askTmplEnv = view _2 -- TA $ \ _n env -> return [env]

localTmplEnv :: (TmplEnv m -> TmplEnv m) -> TmplAct' m a -> TmplAct' m a
localTmplEnv f act = TA $ \ n env ->
  runTmplAct act n (f env)

ttxt :: (Monad m, IsoText a) => a -> TmplAct' m Text
ttxt x = return (x ^. isoText)

tatt :: (Monad m) => String -> TmplAct' m Text
tatt x = return (x ^. to escAttrVal . isoText)

tpt :: (Monad m) => String -> TmplAct' m Text
tpt x = return (x ^. to escPlainText . isoText)

-- ----------------------------------------

instance Monoid (TmplEnv m) where
  mempty = TE (M.empty, M.empty)

  (TE te1) `mappend` (TE te2) =
    TE $ (M.union (fst te1) *** M.union (snd te1)) te2


insAct' :: TmplName -> TmplAct' m Text -> TmplEnv m -> TmplEnv m
insAct' n act e =
  e & theTmplAct n .~ Just act


insTmpl' :: TmplName -> Tmpl -> TmplEnv m -> TmplEnv m
insTmpl' n tmpl e =
  e & theTmplEnv n .~ Just tmpl


insSubTmpl' :: Monad m => TmplName -> Tmpl -> TmplEnv m -> TmplEnv m
insSubTmpl' n tmpl e =
  e & theTmplEnv n .~ Just tmpl
    & theTmplAct n .~ Just applyTmpl'

-- ----------------------------------------

theTEnv :: Iso' (TmplEnv m) ( Map TmplName (TmplAct' m Text)
                            , Map TmplName Tmpl
                            )
theTEnv = iso (\ (TE p) -> p) TE

theTmplAct :: Text -> Lens' (TmplEnv m) (Maybe (TmplAct' m Text))
theTmplAct n = theTEnv . _1 . at n

theTmplEnv :: Text -> Lens' (TmplEnv m) (Maybe Tmpl)
theTmplEnv n = theTEnv . _2 . at n

-- ----------------------------------------

evalPart :: Monad m => TmplPart -> TmplAct' m Text
evalPart (Left  t) = return t
evalPart (Right n) = TA $ \ _n env -> do
  let def = fromMaybe empty $ env ^. theTmplAct "*"
  let act = fromMaybe def   $ env ^. theTmplAct n
  runTmplAct act n env

evalTmpl :: Monad m => Tmpl -> TmplAct' m Text
evalTmpl ts = msum $ map evalPart ts

-- specialized form of runTmplAct
applyTmpl' :: Monad m => TmplAct' m Text
applyTmpl' = TA $ \ n env ->
  runTmplAct (evalTmpl (env ^. theTmplEnv n . _Just)) n env

-- apply a template for a whole sequence of values
-- the template name determines the place where the values
-- are inserted, the action is the insert action for the values,
-- the list contains the values to be inserted
--
-- > insAct' "t6" act
-- > where
-- >   act = applySeq "x" (tatt . show) [1..5]
--
-- installs for the template "t6" an action, which will insert the template
-- for "t6" 5 times, and the template name "x" determines the place, where
-- the values 1..5 of the list will be inserted

applySeq :: Monad m =>
            TmplName -> (a -> TmplAct' m Text) -> [a] -> TmplAct' m Text
applySeq n act xs = msum $ map lact xs
  where
    lact i = localTmplEnv (insAct' n (act i)) $ applyTmpl'

applyCond :: Monad m =>
             Bool -> TmplAct' m a -> TmplAct' m a
applyCond True act = act
applyCond _    _   = empty

applyNotEmpty :: (IsEmpty a, Monad m) =>
                 a -> TmplAct' m Text
applyNotEmpty x = applyCond (not $ isempty x) applyTmpl'

-- ----------------------------------------
-- ----------------------------------------

emptyEnvTmpl :: EnvTmpl m
emptyEnvTmpl = (M.empty, M.empty) ^. from theEnvT

theEnvT :: Iso' (EnvTmpl m) ( Map Text (TmplAct m)
                       , Map Text Template
                       )
theEnvT = iso (\ (EnvTmpl p) -> p) EnvTmpl

theEnvAct :: Text -> Lens' (EnvTmpl m) (Maybe (TmplAct m))
theEnvAct n = theEnvT . _1 . at n

theEnvTmpl :: Text -> Lens' (EnvTmpl m) (Maybe Template)
theEnvTmpl n = theEnvT . _2 . at n

-- ----------------------------------------

-- template variables are rferenced by ${name}

templateRegex :: Regex
templateRegex =
  parseRegexExt $
  "[$][{][A-Za-z][A-Za-z0-9.]*[}]"

parseTemplate' :: Regex -> (String -> String) -> String -> Template
parseTemplate' r f =
  map ( either
        (Left  . (^. from isoString))
        (Right . (^. from isoString) . f)
      )
  . tokenizeRE' r

parseTemplate :: String -> Template
parseTemplate = parseTemplate' templateRegex (init . drop 2)

-- ----------------------------------------

-- evaluate a template with an env containing monadic actions
-- to compute the values for the template variables
-- result is a list of text pieces,
-- a default action can be stored under the fixed name "*"

evalTemplate :: Monad m => Template -> EnvTmpl m -> m [Text]
evalTemplate tmpl env =
  concat <$> mapM evalPart tmpl
  where
    evalPart (Left  t) = return [t]
    evalPart (Right n) = act n env
      where
        act = fromMaybe def                    $ env ^. theEnvAct n
        def = fromMaybe (\ _n _e -> return []) $ env ^. theEnvAct "*"

-- ----------------------------------------

emptyTmpl :: (Monad m) => TmplAct m
emptyTmpl _n _env = return []

-- insert a string as it is
txt :: (Monad m) =>
       String -> TmplAct m
txt s = atxt' (return s)

-- insert simple attribute value
atxt :: (Monad m) =>
        String -> TmplAct m
atxt s = atxt' (return s)

-- insert simple plain text
ptxt :: (Monad m) =>
        String -> TmplAct m
ptxt s = ptxt' (return s)


-- compute a string and insert it
txt' :: (IsoString a, Monad m) =>
         m a -> TmplAct m
txt' = txt'' id

-- compute an attribute value an insert it
atxt' :: (IsoString a, Monad m) =>
         m a -> TmplAct m
atxt' = txt'' escAttrVal

-- copute a text value and insert it
ptxt' :: (IsoString a, Monad m) =>
         m a -> TmplAct m
ptxt' = txt'' escAttrVal


-- escape functions
escAttrVal :: String -> String
escAttrVal = id

escPlainText :: String -> String
escPlainText = id

-- compute a value, convert it into a string, and escape it

txt'' :: (IsoString a, Monad m) =>
         (String -> String) -> m a -> TmplAct m
txt'' esc act _n _env = do
  x <- act
  return [x ^. isoString . to esc . isoText]

applyTmpl :: (Monad m) => TmplAct m
applyTmpl n env =
  evalTemplate (env ^. theEnvTmpl n . _Just) env

-- ----------------------------------------

-- insert an action for a template variable
insAct :: Monad m =>
              Text -> (TmplAct m) -> EnvTmpl m -> EnvTmpl m
insAct n act env = env & theEnvAct n .~ Just act

-- insert a sub template
insTmpl :: Monad m =>
              Text -> Template -> EnvTmpl m -> EnvTmpl m
insTmpl n tmpl env = env & theEnvTmpl n .~ Just tmpl

-- insert a sub template with default action apply
--
-- useful for easy modularization of complex templates

insSubTmpl :: Monad m =>
              Text -> Template -> EnvTmpl m -> EnvTmpl m
insSubTmpl n tmpl env =
  env & theEnvTmpl n .~ Just tmpl
      & theEnvAct  n .~ Just applyTmpl

-- ----------------------------------------

{- mini test -}
e1 :: EnvTmpl IO
e1 =
  emptyEnvTmpl
  & insAct "*" (\ n _env -> do
                   putStrLn $ "unknown template var ignored: " ++ n ^.isoString
                   return []
               )
  & theEnvAct "xxx" .~ Just (\ _n _env -> return ["yyy"])
  & insAct "theo" (\ n _env -> return ["hello", n, "bye"])
  & insAct "t2" (\ n env -> do
                    print n
                    print (env ^. theEnvT . _2)
                    applyTmpl n env
                )
  & insAct "t3" applyTmpl
  & insTmpl "t1" t1
  & insTmpl "t2" t2
  & insTmpl "t3" t3

t1 :: Template
t1 = parseTemplate "abc${xxx}x${t2}yz${theo}123${t3}456${unknown}789"

t2 :: Template
t2 = parseTemplate "nested ${xxx} template${xxx}"

t3 :: Template
t3 = parseTemplate "another nested template"

ttt :: IO [Text]
ttt = evalTemplate t1 e1
-- -}

-- ----------------------------------------
-- ----------------------------------------

{- mini test -}
ee1 :: TmplEnv IO
ee1 =
  mempty
  & insSubTmpl' "t1" t1'
  & insSubTmpl' "t2" t2'
  & insSubTmpl' "t3" t3'
  & insAct' "*" (do n <- askTmplName
                    liftTA $ putStrLn $ "unknown template var ignored: " ++ n ^.isoString
                    empty
                )
  & insAct' "xxx" (tatt "yyy")
  & insAct' "theo" (tatt "hello " <|> askTmplName <|> tatt " bye")
  & insTmpl' "t4" t4'
  & insAct' "t4" (applyTmpl' <|> applyTmpl')
  & insTmpl' "t5" t5'
  & insAct' "t5" (applySeq' [1..5])
  & insTmpl' "t6" t6'
  & insAct' "t6" (applySeq "x" (tatt . show) ['a'..'f'])

condTxt :: Text -> TmplAct' IO Text
condTxt t = applyCond (isempty t) (tatt (t ^. isoString))

applySeq' :: [Int] -> TmplAct' IO Text
applySeq' xs = msum $ map act xs
  where
    act i = localTmplEnv (insAct' "no" (tatt (show i))) $ applyTmpl'

-- applySeq' :: TmplName -> (a -> TmplAct' IO Text) -> [a] -> TmplAct' IO Text
{- }
  & insAct' "*" (TA $ \ n _env -> do
                    putStrLn $ "unknown template var ignored: " ++ n ^.isoString
                    return []
               )

  & theEnvAct "xxx" .~ Just (\ _n _env -> return ["yyy"])
  & insAct "theo" (\ n _env -> return ["hello", n, "bye"])
  & insAct "t2" (\ n env -> do
                    print n
                    print (env ^. theEnvT . _2)
                    applyTmpl n env
                )
  & insAct "t3" applyTmpl
  & insTmpl "t1" t1
  & insTmpl "t2" t2
  & insTmpl "t3" t3
-- -}

t1' :: Template
t1' = parseTemplate "abc${xxx}x${t2}yz${theo}123${t3}456${unknown}789${t5}++${t6}"

t2' :: Template
t2' = parseTemplate "nested ${xxx} template${xxx}"

t3' :: Template
t3' = parseTemplate "two times the nested template ${t4} (t4)"

t4' :: Template
t4' = parseTemplate "((( ${xxx} )))"

t5' :: Template
t5' = parseTemplate "[${no}]"

t6' :: Template
t6' = parseTemplate " ${x} "

xxx :: IO [Text]
xxx = runTmplAct applyTmpl' "t1" ee1


-- ----------------------------------------
