-- 
-- Copyright (c) Tuomo Valkonen 2006.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
--
-- Many thanks to Tuomo Valkonen

-- ------------------------------------------------------------

module Language.Lua.Parser
    ( parse_chunk
    , ParseError
    )
where

import Control.Monad 		( liftM )

import Language.Lua.AST
import Language.Lua.Token     	( Token(..)
                                , TokenStream
                                )
import Text.Parsec
import Text.Parsec.Expr

-- ------------------------------------------------------------
{-
import Debug.Trace

debug x = trace (show x) x
-- -}
-- ------------------------------------------------------------

parse_chunk :: FilePath -> TokenStream -> Either ParseError Block
parse_chunk = parse $ do
    r <- block
    eof
    return r

-- ------------------------------------------------------------

type Parser r = Parsec [(SourcePos, Token)] () r

-- ------------------------------------------------------------

mytoken :: (Token -> Maybe a) -> Parser a
mytoken test = 
    token showtok postok testtok
    where
        showtok (_pos,  tok) = show tok
        postok  ( pos, _tok) = pos
        testtok (_pos,  tok) = test tok

tok_string :: Parser String
tok_string = mytoken (\f -> case f of
                                (TokString a) -> Just a
                                _ -> Nothing)

tok_number :: Parser Double
tok_number = mytoken (\f -> case f of
                                (TokNumber a) -> Just a
                                _ -> Nothing)

tok_identifier :: Parser String
tok_identifier = mytoken (\f -> case f of
                                (TokIdentifier a) -> Just a
                                _ -> Nothing)
{-
tok_reserved :: Parser String
tok_reserved = mytoken (\f -> case f of
                                (TokReserved a) -> Just a
                                _ -> Nothing)

tok_special :: Parser String
tok_special = mytoken (\f -> case f of
                                (TokSpecial a) -> Just a
                                _ -> Nothing)

tok_operator :: Parser String
tok_operator = mytoken (\f -> case f of
                                (TokOperator a) -> Just a
                                _ -> Nothing)
-}
reserved :: String -> Parser ()
reserved s = mytoken (\f -> case f of
                                (TokReserved t) | s == t -> Just ()
                                _ -> Nothing)

special :: String -> Parser ()
special s = mytoken (\f -> case f of
                                (TokSpecial t) | s == t -> Just ()
                                _ -> Nothing)

operator :: String -> Parser ()
operator s = mytoken (\f -> case f of
                                (TokOperator t) | s == t -> Just ()
                                _ -> Nothing)

parens :: Parser a -> Parser a
parens p   = between (special "(") (special ")") p

braces :: Parser a -> Parser a
braces p   = between (special "{") (special "}") p

brackets :: Parser a -> Parser a
brackets p = between (special "[") (special "]") p

-----------

block :: Parser Block
block = liftM Block $ many $ do
    s <- stat
    option () (special ";")
    return s

stat :: Parser Stmt
stat = choice [
    stmt_do,
    stmt_while,
    stmt_repeat,
    stmt_if,
    stmt_return,
    stmt_break,
    stmt_for,
    stmt_function,
    stmt_local,
    stmt_assignment_or_funccall
    ]


stmt_do :: Parser Stmt
stmt_do = do
    reserved "do"
    b <- block
    reserved "end"
    return $ SDo b

stmt_while :: Parser Stmt
stmt_while = do
    reserved "while"
    e <- exp_exp
    reserved "do"
    b <- block
    reserved "end"
    return $ SWhile e b

stmt_repeat :: Parser Stmt
stmt_repeat = do
    reserved "repeat"
    b <- block
    reserved "until"
    e <- exp_exp
    return $ SUntil e b
    
stmt_if :: Parser Stmt
stmt_if = do
    reserved "if"
    e <- exp_exp
    reserved "then"
    b <- block
    eb <- many $ do
              reserved "elseif"
              e_ <- exp_exp
              reserved "then"
              b_ <- block
              return (e_, b_)
    df <- option Nothing $ do
              reserved "else"
              liftM Just $ block
    reserved "end"
    return $ SIf ((e, b):eb) df

stmt_return :: Parser Stmt
stmt_return = do
    reserved "return"
    liftM SReturn $ option [] explist1

stmt_break :: Parser Stmt
stmt_break = do
    reserved "break"
    return SBreak

stmt_for :: Parser Stmt
stmt_for = do
    reserved "for"
    (ids, gen) <- (try stmt_for1 <|> stmt_for2)
    reserved "do"
    b <- block
    reserved "end"
    return $ SFor ids gen b
    where
       stmt_for1 = do
           id' <- tok_identifier
           special "="
           from <- exp_exp
           special ","
           to <- exp_exp
           step <- option Nothing $ do
                       special ","
                       liftM Just exp_exp
           return $ ([id'], ForNum from to step)
           
       stmt_for2 = do
           ids <- sepBy1 tok_identifier (special ",")
           reserved "in"
           exs <- explist1
           return $ (ids, ForIter exs)

stmt_function :: Parser Stmt
stmt_function = do
    reserved "function"
    (n, a) <- funcname
    ref <- tolvar $ foldl EFieldRef 
                          (EVar $ head n) 
                          (map EString $ tail n)
    b <- funcbody
    case (a, b) of
        (False, _) ->
            return $ SAssignment [ref] [b]
        (True, EFunction pn va bb) ->
            return $ SAssignment [ref] [EFunction ("self":pn) va bb]
        (_, _) ->
            error "stmt_function: error in parse"

stmt_local :: Parser Stmt
stmt_local = do
    reserved "local"
    do
        reserved "function" 
        n <- tok_identifier
        b <- funcbody
        return $ SLocalDef [n] [b]
      <|> do
        ns <- namelist1
        exs <- option [] $ do
                  special "="
                  explist1
        return $ SLocalDef ns exs

stmt_assignment_or_funccall :: Parser Stmt
stmt_assignment_or_funccall = do
    ex <- primaryexp
    case ex of
        -- Function call
        (ECall _ _) -> return $ SAssignment [] [ex]
        (EMemberCall _ _ _) -> return $ SAssignment [] [ex]
        -- Assignment
        (EVar n) -> stmt_assignment' [LVar n]
        (EFieldRef t f) -> stmt_assignment' [LFieldRef t f]
        _ -> fail "Invalid statement"
        
stmt_assignment' :: [LValue] -> Parser Stmt
stmt_assignment' lhs = 
    do
        special ","
        lv <- lvalue
        stmt_assignment' (lv:lhs)
      <|> do
        special "="
        vals <- explist1
        return $ SAssignment (reverse lhs) vals

-----------

lvalue :: Parser LValue
lvalue = do
    ex <- primaryexp
    tolvar ex

tolvar :: Expr -> Parser LValue
tolvar ex = do
    case ex of
        (EVar n) -> return $ LVar n
        (EFieldRef t f) -> return $ LFieldRef t f
        _ -> fail "Invalid lvalue"

-----------

exp_anonfunction :: Parser Expr
exp_anonfunction = do
    reserved "function"
    funcbody

funcbody :: Parser Expr
funcbody = do
    (pn, vararg) <- parens parlist0
    b <- block
    reserved "end"
    return $ EFunction pn vararg b


funcname :: Parser ([String], Bool)
funcname = do
    n' <- sepBy1 tok_identifier (special ".")
    do
        special ":"
        i <- tok_identifier
        return (n' ++ [i], True)
      <|>
        return (n', False)

namelist1 :: Parser [String]
namelist1 = sepBy1 tok_identifier (special ",")

explist1 :: Parser [Expr]
explist1 = sepBy1 exp_exp (special ",")

-----------

prefixexp :: Parser Expr
prefixexp = choice [
    tok_identifier >>= return . EVar,
    parens exp_exp
    ]

primaryexp :: Parser Expr
primaryexp = do
    pfx <- prefixexp
    more pfx
    where
        more i = do { e <- dot_index i; more e }
             <|> do { e <- brace_index i; more e }
             <|> do { e <- member_call i; more e }
             <|> do { e <- fcall i; more e}
             <|> return i

        dot_index e = do
            special "."
            id' <- tok_identifier
            return $ EFieldRef e (EString id')
            
        brace_index e = liftM (EFieldRef e) $ brackets exp_exp
        
        member_call e = do
            special ":"
            id' <- tok_identifier
            arg <- funcargs
            return $ EMemberCall e id' arg
            
        fcall e = liftM (ECall e) funcargs

simpleexp :: Parser Expr
simpleexp
    = choice
      [ liftM ENumber $ tok_number
      , liftM EString $ tok_string
      , reserved "true" >> return (EBool True)
      , reserved "false" >> return (EBool False)
      , reserved "nil" >> return ENil
      , special "..." >> return EEllipsis
      , tableconstructor
      , exp_anonfunction
      , primaryexp
      ]

exp_exp :: Parser Expr
exp_exp =
    buildExpressionParser (map (map f) ops) simpleexp
    where
        f (s, True, _) = 
            Prefix (do{ operator s; return $ \e -> EUnOp s e})
        f (s, False, a) = 
            Infix (do{ operator s; return $ \e' f' -> EBinOp s e' f'}) a


ops :: [[(String, Bool, Assoc)]]
ops = [
    [("^",   False,  AssocRight)],
    [("-",   True,   undefined),
     ("#",   True,   undefined),
     ("not", True,   undefined)],
    [("/",   False,  AssocLeft),
     ("*",   False,  AssocLeft)],
    [("-",   False,  AssocLeft),
     ("+",   False,  AssocLeft)],
    [("..",  False,  AssocRight)],
    [("==",  False,  AssocLeft),
     ("~=",  False,  AssocLeft),
     (">=",  False,  AssocLeft),
     ("<=",  False,  AssocLeft),
     (">",   False,  AssocLeft),
     ("<",   False,  AssocLeft)],
    [("and", False,  AssocLeft)],
    [("or",  False,  AssocLeft)]
    ]

-----------

funcargs = do
        (parens $ option [] explist1)
    <|> (liftM (:[]) $ tableconstructor)
    <|> (liftM (\s -> [EString s]) $ tok_string)

parlist0 :: Parser ([String], Bool)
parlist0 = parlist1
           <|>
           return ([], False) 

parlist1 :: Parser ([String], Bool)
parlist1 =
    do  i <- tok_identifier
        (n, a) <- do { special ","; parlist1 } 
                  <|> return ([], False)
        return (i:n, a)
    <|>
    do  special "..."
        return ([], True)

-----------

tableconstructor = liftM ETableCons $ braces fieldlist

fieldlist = sepEndBy field fieldsep

field = do
    field_1 <|> field_2 <|> field_3
    where 
        field_1 = do
            f <- brackets exp_exp
            special "="
            v <- exp_exp
            return (Just f, v)
        field_2 = do 
            id' <- try $ do i <- tok_identifier
                            special "="
                            return i
            v <- exp_exp
            return (Just (EString id'), v)
        field_3 = do
            v <- exp_exp
            return (Nothing, v)

fieldsep :: Parser ()
fieldsep = special "," <|> special ";"

-- ------------------------------------------------------------