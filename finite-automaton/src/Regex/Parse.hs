module Regex.Parse
    ( parseRegex )
where

import Regex.Core
import Text.ParserCombinators.Parsec

import Data.Maybe
import Data.List
import Data.Set.Simple (mkSet)

allChars :: [Char]
allChars = ['\0' .. '\255']             -- 8 bit alphabet

rePrime :: Parser RE
rePrime
    = ( do
        c <- satisfy $ not . (`elem` ".*+?|[]()\\")
        return (REsymbol c)
      )
      <|>
      ( do
        char '\\'
        c <- char '\\' >> reEscape
        return (REsymbol c)
      )
      <|>
      ( -- single char wildcard
        char '.' >> return (REsymset $ mkSet allChars)
      )
      <|>
      between (char '(') (char ')') reAlt
      <|>
      ( do
        cs <- between (char '[') (char ']') reSymSet
        return $ REsymset (mkSet cs)
      )
      <?> "simple char"

reSymSet :: Parser [Char]
reSymSet
    = ( do
        char '^'
        rs <- reSymSet'
        return $ allChars \\ rs
      )
      <|>
      reSymSet'

reSymSet' :: Parser [Char]
reSymSet'
    = do
      css <- many (symInterval)
      return $ foldr union [] css
    where

    symChar :: Parser Char
    symChar
        = ( do
            char '\\'
            reEscape
          )
          <|>
          noneOf "]"

    symInterval :: Parser [Char]
    symInterval
        = do
          c1 <- symChar
          symIntervalRest c1

    symIntervalRest :: Char -> Parser [Char]
    symIntervalRest c1
        = option [c1]
          ( do
            char '-'
            c2 <- symChar
            return [c1 .. c2]
          )

reEscape :: Parser Char
reEscape
    = ( do
        char 'n'
        return '\n'
      )
      <|>
      ( do
        char 't'
        return '\t'
      )
      <|>
      ( do
        char 'r'
        return '\r'
      )
      <|>
      ( do                              -- extend this for other escape sequences, e.g. '\32' od '\x20'
        c <- satisfy (const True)
        return c
      )


reFactor :: Parser RE
reFactor
    = do
      re' <- rePrime
      reFactor' re'
    where
    reFactor' :: RE -> Parser RE
    reFactor' re'
        = option re'
          ( ( do
              c <- oneOf "*+?"
              reFactor' ( (fromJust . lookup c $ [('*',RErep), ('+',RErep1),('?',REopt)]) re')
            )
            <|>
            ( do
              char '{'
              bds <- bounds
              char '}'
              return (reBounds re' bds)
            )
          )
    bounds      :: Parser (Int, Maybe (Maybe Int))
    bounds      
        = do
          n <- digits
          m <- upperBound
          return (n, m)

    upperBound  :: Parser (Maybe (Maybe Int))
    upperBound
        = option Nothing
          ( do
            char ','
            m <- option Nothing
                 ( do
                   m' <- digits
                   return (Just m')
                 )
            return (Just m)
          )

    digits      :: Parser Int
    digits
        = do
          s <- many1 digit
          return (foldl1 (\ x y -> 10 * x + y) . map (\ c -> fromEnum c - fromEnum '0') $ s)

    reBounds re' (n, Nothing)           = RErepN re' n
    reBounds re' (n, Just Nothing)      = RErepNstar re' n
    reBounds re' (n, Just (Just m))     = RErepN2M re' n m

reSeq :: Parser RE
reSeq
    = option (REepsilon)
      ( do
        res <- many1 reFactor
        return $ foldr1 REseq res
      )

reAlt :: Parser RE
reAlt
    = do
      re1 <- reSeq
      res <- many ( do
                    char '|'
                    reSeq
                  )
      return $ foldr1 REalt (re1 : res)

regex      :: Parser RE
regex
    = do
      re' <- reAlt
      eof
      return re'

parseRegex :: String -> Either String RE
parseRegex
    = either (Left . show) Right
      .
      parse regex ""

-- ----------------------------------------
