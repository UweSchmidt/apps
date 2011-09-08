{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Language.Tcl.Expr.Parser
where

import Data.Char                        ( digitToInt )

import Language.Tcl.Value               ( mkI, mkD, mkS )
import Language.Tcl.Expr.AbstractSyntax

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language   ( emptyDef )
import qualified Text.Parsec.Token     	as P

-- ------------------------------------------------------------

type ExprParser = Parsec String ()

type TokenParser = P.TokenParser ()

-- ------------------------------------------------------------

lexer :: TokenParser
lexer
    = P.makeTokenParser emptyDef

parens :: ExprParser a -> ExprParser a
parens
    = P.parens lexer

natural :: ExprParser Integer
natural
    = P.lexeme lexer natNum
{-
    = P.natural lexer
-}

integer :: ExprParser Integer
integer
    = P.integer lexer

float :: ExprParser Double
float
    = P.float lexer

name :: ExprParser String
name
    = P.identifier lexer

comma :: ExprParser String
comma
    = P.comma lexer

stringLiteral :: ExprParser String
stringLiteral
    = P.stringLiteral lexer

reservedOp :: String -> ExprParser ()
reservedOp
    = P.reservedOp lexer

whiteSpace :: ExprParser ()
whiteSpace
    = P.whiteSpace lexer

-- ------------------------------------------------------------

number :: Integer -> ExprParser Char -> ExprParser Integer
number base baseDigit
    = do digits <- many1 baseDigit
         let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
         n `seq` return n

hexNum :: ExprParser Integer
hexNum
    = try $ string "0x" >> number 16 hexDigit

decNum :: ExprParser Integer
decNum
    = number 10 digit

octNum :: ExprParser Integer
octNum
    = try $ char '0' >> option 'o' (char 'o') >> number 8 octDigit

binNum :: ExprParser Integer
binNum
    = try $ string "0b" >> number 2 (oneOf "01")

natNum :: ExprParser Integer
natNum
    = hexNum <|> octNum <|> binNum <|> decNum

-- ------------------------------------------------------------

expr :: ExprParser TclExpr
expr
    = do e1 <- expr1
         ifexpr e1
    where
      ifexpr cnd
          = option cnd $
            do reservedOp "?"
               e2 <- expr
               reservedOp ":"
               e3 <- expr
               return $ TExpr "?:" [cnd, e2, e3]

expr1 :: ExprParser TclExpr
expr1
    = buildExpressionParser table term
      <?> "expression"
    where
    table
        = [ [ prefix "-"
            , prefix "+"
            , prefix "!"
            , prefix "~"
            ]
          , [ binary "*"  AssocLeft
            , binary "/"  AssocLeft
            , binary "%"  AssocLeft
            ]
          , [ binary "+"  AssocLeft
            , binary "-"  AssocLeft
            ]
          , [ binary ">>" AssocLeft
            , binary "<<" AssocLeft
            ]
          , [ binary "<=" AssocLeft
            , binary "<"  AssocLeft
            , binary ">=" AssocLeft
            , binary ">"  AssocLeft
            ]
          , [ binary "==" AssocLeft
            , binary "!=" AssocLeft
            ]
          , [ binary "&"  AssocLeft
            ]
          , [ binary "^"  AssocLeft
            ]
          , [ binary "|"  AssocLeft
            ]
          , [ binary "&&" AssocLeft
            ]
          , [ binary "||" AssocLeft
            ]
          ]
        where
          prefix op
              = Prefix ( do reservedOp op
                            return mk1 )
              where
                mk1 e1
                    = TExpr op [e1]
          binary op assoc
              = Infix ( do reservedOp op
                           return mk2 ) assoc
              where
                mk2 e1 e2
                    = TExpr op [e1, e2]

term :: ExprParser TclExpr
term
    = parens expr
      <|>
      ( try float     >>= return . TConst . mkD )
      <|>
      ( natural       >>= return . TConst . mkI )
      <|>
      ( stringLiteral >>= return . TConst . mkS )
      <|>
      call

call :: ExprParser TclExpr
call
    = do n <- name
         option (TConst . mkS $ n) $
           parens exprList >>= return . TExpr n
    where
      exprList
          = do e1 <- expr
               es <- many ( comma >> expr )
               return $ e1 : es

-- ------------------------------------------------------------
--
-- the main Tcl parsers

parseTclExpr :: String -> Either ParseError TclExpr
parseTclExpr
    = parse (eofP $ whiteSpace >> expr) ""

-- ------------------------------------------------------------

eofP :: ExprParser r -> ExprParser r
eofP ps
    = do r <- ps
	 eof
	 return r

-- ------------------------------------------------------------
