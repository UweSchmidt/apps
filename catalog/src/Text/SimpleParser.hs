module Text.SimpleParser
  ( module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module Text.SimpleParser
  )
where

import Data.Prim.Prelude
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char hiding (noneOf)
import qualified
       Text.Megaparsec.Char as P

-- --------------------
--
-- simple String parser

type SP = Parsec Void String

-- --------------------

infixr 5 <++>

(<++>) :: (Applicative f, Monoid b) => f b -> f b -> f b
p1 <++> p2 = (<>) <$> p1 <*> p2

{-# INLINE (<++>) #-}

anyStringThen' :: SP a -> SP (String, a)
anyStringThen' p =
  try ( do ps <- p
           return ("", ps)
      )
  <|>
  do c        <- anyChar
     (cs, ps) <- anyStringThen' p
     return (c : cs, ps)

-- substitute for regex ".*xxx"
--
-- example: split a file extension from a path
--
-- parseMaybe anyStringThen' (string ".jpg" <* eof) "abc.jpgf.jpg"
--    -> Just ("abc.jpgf", ".jpg")

anyStringThen :: SP String -> SP String
anyStringThen p = uncurry (++) <$> anyStringThen' p

splitSuffix :: SP a -> SP (String, a)
splitSuffix p = anyStringThen' (p <* eof)

withSuffix :: SP String -> SP String
withSuffix p = uncurry (++) <$> splitSuffix p

anyString :: SP String
anyString = many anyChar

noneOf' :: String -> SP Char
noneOf' = P.noneOf

-- --------------------

matchP :: SP a -> String -> Bool
matchP p = toBool . parseMaybe p

toBool :: Maybe a -> Bool
toBool = maybe False (const True)

matchPred :: (a -> Bool) -> a -> Maybe a
matchPred p x
  | p x       = Just x
  | otherwise = Nothing

eqNoCase :: String -> String -> Bool
eqNoCase = (==) `on` map toLower

-- ----------------------------------------
