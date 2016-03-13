{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.SimpleTemplate
where

import Data.String.QQ

-- import Data.Functor.Identity

import Data.Prim.Prelude
import qualified Data.Map.Strict as M

type Template = [Either Text Text]

newtype Env m = Env (Map Text (Env m -> m [Text]))


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

parseTemplate :: String -> [Either Text Text]
parseTemplate = parseTemplate' templateRegex (init . drop 2)


evalTemplate :: Monad m => Env m -> Template -> m [Text]
evalTemplate env@(Env mp) tmpl =
  concat <$> mapM evalPart tmpl
  where
    evalPart (Left  t) = return [t]
    evalPart (Right n) = act env
      where
        act = fromMaybe (const $ return []) $ M.lookup n mp

txt :: Monad m => Text -> Env m -> Cmd [Text]
txt s =

{- mini test }
e1 :: Env Identity
e1 = Env $ M.fromList [("xxx", const $ return ["yyy"])]

t1 :: [Either Text Text]
t1 = parseTemplate "abc${xxx}xyz"

ttt :: [Text]
ttt = runIdentity $ evalTemplate e1 t1

-- -}

{- }
foo :: String
foo = [s|
Well here is a
     multi-line string!
|]

-- -}
