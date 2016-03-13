{-# LANGUAGE OverloadedStrings #-}

-- a very simple template substitution system
-- the substitution can run in an arbitrary monad

module Text.SimpleTemplate
where

import qualified Data.Map.Strict as M
import           Data.Prim.Prelude

-- ----------------------------------------

type Template = [Either Text Text]

newtype Env m = Env (Map Text (Text -> Env m -> m [Text]))

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

evalTemplate :: Monad m => Template -> Env m -> m [Text]
evalTemplate tmpl env@(Env mp) =
  concat <$> mapM evalPart tmpl
  where
    evalPart (Left  t) = return [t]
    evalPart (Right n) = act n env
      where
        act = fromMaybe def                    $ M.lookup n   mp
        def = fromMaybe (\ _n _e -> return []) $ M.lookup "*" mp

-- ----------------------------------------

-- insert a string as it is
txt :: (Monad m) =>
       String -> Text -> Env m -> m [Text]
txt s = atxt' (return s)

-- insert simple attribute value
atxt :: (Monad m) =>
        String -> Text -> Env m -> m [Text]
atxt s = atxt' (return s)

-- insert simple plain text
ptxt :: (Monad m) =>
        String -> Text -> Env m -> m [Text]
ptxt s = ptxt' (return s)


-- compute a string and insert it
txt' :: (IsoString a, Monad m) =>
         m a -> Text -> Env m -> m [Text]
txt' = txt'' id

-- compute an attribute value an insert it
atxt' :: (IsoString a, Monad m) =>
         m a -> Text -> Env m -> m [Text]
atxt' = txt'' escAttrVal

-- copute a text value and insert it
ptxt' :: (IsoString a, Monad m) =>
         m a -> Text -> Env m -> m [Text]
ptxt' = txt'' escAttrVal


-- escape functions
escAttrVal :: String -> String
escAttrVal = id

escPlainText :: String -> String
escPlainText = id

-- compute a value, convert it into a string, and escape it

txt'' :: (IsoString a, Monad m) =>
         (String -> String) -> m a -> Text -> Env m -> m [Text]
txt'' esc act _n _env = do
  x <- act
  return [x ^. isoString . to esc . isoText]

-- ----------------------------------------

insertTmpl :: Monad m =>
              Text -> (Text -> Env m -> m [Text]) -> Env m -> Env m
insertTmpl n act (Env m) =
  Env $ M.insert n act m

-- ----------------------------------------

{- mini test }
e1 :: Env Identity
e1 = Env $ M.fromList [("xxx", const $ return ["yyy"])]

t1 :: [Either Text Text]
t1 = parseTemplate "abc${xxx}xyz"

ttt :: [Text]
ttt = runIdentity $ evalTemplate e1 t1
-- -}

-- ----------------------------------------
