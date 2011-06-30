{- OPTIONS -XTypeSynonymInstances  -XFlexibleContexts -XFlexibleInstances -XMultiParamTypeClasses #-}
{-# OPTIONS -XFlexibleContexts #-}

module Language.Common.EvalOptions
where

import Control.Arrow          ( (>>>) )
import Control.Applicative    ( )              -- (<$>))
import Control.Monad.Identity

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos

-- ------------------------------------------------------------

type OptParser s u = Parsec s u ()

evalOptions :: (Stream s Identity t) => OptParser s u -> u -> s -> Either String (u, s)
evalOptions optionParser initialState
    = runP ( do optionParser
                opts <- getState
                args <- getInput
                return (opts, args)
            )  initialState ""
      >>>
      either (Left . unlines . map messageString . errorMessages) Right

-- ------------------------------------------------------------

nextArg :: (Stream s m a, Show a) => ParsecT s u m a
nextArg = nextArgWith (const True)

nextArgWith :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m a
nextArgWith p
    = tokenPrim showArg nextPos testArg
    where
      showArg x           = show x
      testArg x           = if p x then Just x else Nothing
      nextPos pos _x _xs  = updatePosChar pos ' '	-- don't count chars but count args

-- ------------------------------------------------------------
--
-- the elementary option parsers

isOpt :: (Stream s m a, Show a) => (a -> Bool) -> (u -> u) -> ParsecT s u m ()
isOpt name value
    = nextArgWith name
      >>
      updateState value

isArgOpt :: (Stream s m t, Show t) => (t -> Bool) -> (t -> u -> u) -> ParsecT s u m ()
isArgOpt name optValue
    = nextArgWith name
      >>=
      argVal optValue

isCheckedArgOpt :: (Stream s m a1, Show a1) =>
                   (a1 -> Bool) -> (a1 -> Bool, String) -> (a1 -> u -> u) -> ParsecT s u m ()
isCheckedArgOpt name valueCheck optValue
    = nextArgWith name
      >>=
      argCheckedVal valueCheck optValue

isIllegalOpt :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m b
isIllegalOpt name
    = nextArgWith name
      >>=
      illegalVal "unknown option"

-- ------------------------------------------------------------

argVal :: (Stream s m t, Show a, Show t) => (t -> u -> u) -> a -> ParsecT s u m ()
argVal = argCheckedVal (const True, "")

argCheckedVal :: (Stream s m a1, Show a2, Show a1) =>
                 (a1 -> Bool, String) -> (a1 -> u -> u) -> a2 -> ParsecT s u m ()
argCheckedVal (valuePred, valueError) optionValue optName
    = ( do v <- nextArg
           if valuePred v
              then modifyState (optionValue v)
              else illegalVal valueError v
      )
      <|>
      illegalVal "missing argument for option" optName

illegalVal :: Show a1 => String -> a1 -> ParsecT s u m a
illegalVal msg optName
    = parserFail (msg ++ ": " ++ show optName)

-- ------------------------------------------------------------
--
-- option parser combinator for parsing all leading options

options :: [OptParser s u] -> OptParser s u
options os
    = foldr (\ o1 os' -> (o1 >> options os) <|> os') (return ()) os

-- ------------------------------------------------------------
--
-- parsers for options as list of strings

type ArgParser   u = OptParser [String] u

evalProgArgs :: ArgParser u -> u -> [String] -> Either String (u, [String])
evalProgArgs = evalOptions

-- ------------------------------------------------------------
