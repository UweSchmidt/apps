{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- a very simple template substitution system
-- the substitution can run in an arbitrary monad

module Text.SimpleTemplate
where

import qualified Data.Map.Strict as M
import           Data.Prim.Prelude

-- ----------------------------------------

type Template = [Either Text Text]

newtype EnvTmpl m = EnvTmpl ( Map Text (TmplAct m)
                    , Map Text Template
                    )

type TmplAct m = Text -> EnvTmpl m -> m [Text]

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
