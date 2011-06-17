module TclParser
where

import Data.Char
import Data.Monoid

import Text.Parsec

-- ------------------------------------------------------------

type TclParser = Parsec String ()

newtype TclProg
    = TclProg [TclCmd]
      deriving (Show)

data TclCmd
    = TclCmd [TclArg]	-- list contains at least 1 arg
      deriving (Show)

newtype TclArg
    = TclArg [TclSubst]
      deriving (Show)

data TclSubst
    = TLit  String
    | TVar  String
    | TEval TclProg
      deriving (Show)

{-
instance Show TclProg where
    show (TclProg cl) = "\n" ++ concatMap show cl

instance Show TclCmd where
    show (TclCmd (c : al)) = show c ++ concatMap ((" " ++) . show) al ++ "\n"

instance Show TclArg where
    show (TclArg s) = concatMap show s

instance Show TclSubst where
    show (TLit  s)
	=  concatMap escapeChar s
	   where
	   escapeChar c
	       | isAlphaNum c
		   = [c]
	       | c `elem` " {}[]$\\"
                   = "\\" ++ [c]
	       | otherwise
		   = reverse . tail . reverse . tail . show $ c
    show (TVar  v)
	= "$" ++ escapeVarName v
	  where
	  escapeVarName s
	      |  all (\ c -> isAlphaNum c || c == '_' || c == ':') s
                  = s
	      | otherwise
		  = "{" ++ s ++ "}"

    show (TEval p)
	= "[" ++ show p ++ "]"
-}
-- ------------------------------------------------------------

tprog :: TclParser TclProg
tprog
    = tprog' ""

tprog' :: String -> TclParser TclProg
tprog' na
    = do
      cs <- tcmds na
      return $ TclProg cs

tcmds :: String -> TclParser [TclCmd]
tcmds na
    = do
      c  <- tcmd na
      cs <- option [] $ do nl1
			   tcmds na
      return $ c ++ cs

tcmd :: String -> TclParser [TclCmd]
tcmd na
    = do
      ws0
      option [] $ do
		  c  <- targ  na
		  al <- targs na
		  return $ [TclCmd (c : al)]

targs :: String -> TclParser [TclArg]
targs na
    = option [] $
      do
      ws1
      ( targs1 <|> return [] )
    where
    targs1
	= do
	  a  <- targ  na
	  as <- targs na
	  return $ a : as

-- ------------------------------------------------------------
--
-- 3 variants of args
-- .1 unquoted sequence of chars
-- .2 double quoted sequence of chars
-- .3 text within braces
-- .4 commands within brackets

ws  = " \t"
nl  = "\n\r;"
vce ="$[\\"	-- variable, command, escape
br  = "{"
dq  = "\""

targ :: String -> TclParser TclArg
targ na
    = do
      xs <- tcharsArg na <|> tdquoteArg na <|> tbraceArg na
      return $ TclArg xs

tcharsArg :: String -> TclParser [TclSubst]
tcharsArg na
    = do
      s1 <- (tchar1 <|> tesc <|> tvar <|> tbracket)
      xs <- many (tchar (na ++ vce ++ ws ++ nl) <|> tesc <|> tvar <|> tbracket)
      return $ s1 : xs
    where
    tchar1
	= do
	  c <-        noneOf $ na ++ br ++ vce ++ ws ++ nl
	  s <- many $ noneOf $ na ++       vce ++ ws ++ nl
	  return $ TLit (c : s)

tdquoteArg :: String -> TclParser [TclSubst]
tdquoteArg na
    = do
      s <- between (char '\"') (char '\"') $
	   many $
	   tchar (dq ++ vce) <|> tesc <|> tvar <|> tbracket
      noExtraChar na
      return s

tbraceArg  :: String -> TclParser [TclSubst]
tbraceArg na
    = do
      a <- tbrace
      noExtraChar na
      return [a]

-- ------------------------------------------------------------

tchar :: String -> TclParser TclSubst
tchar notAllowed
    = do
      s <- many1 $ noneOf notAllowed
      return $ TLit s

tesc :: TclParser TclSubst
tesc
    = do
      char '\\'
      ( do
	c <- escChar
        return $ TLit [c] )
	<|>
	( do
	  c <- anyChar
	  return $ TLit [c]
	)
    where
    escChar	-- octal, hex and unicode escapes are not handled
	=  ( do
	     c <- oneOf "abfnrtv"
	     return $ read $ "\'\\" ++ [c] ++ "\'"
	   )
      
tvar :: TclParser TclSubst
tvar
    = do
      char '$'
      option (TLit "$") $ do
			  v <- varName
			  return $ TVar v
    where
    varName		-- not implemented: array vars
	= ( do
	    xs <- many1 (many1 (letter <|> digit <|> char '_') <|> namespaceSep)
	    return $ concat xs
	  )
	  <|>
	  ( do
	    s <- between (char '{') (char '}') $ many1 $ noneOf "}"
	    return s
	  )
	where
	namespaceSep
	    = do
	      c1 <- char ':'
	      cs <- many1 $ char ':'
	      return $ c1 : cs

tbracket :: TclParser TclSubst
tbracket
    = do
      s <- between (char '[') (char ']') $ tprog' "]"
      return $ TEval s
      

tbrace :: TclParser TclSubst
tbrace
    = between (char '{') (char '}') $
      do
      s <- braceContent
      return $ TLit s

-- ------------------------------------------------------------

braceContent :: TclParser String
braceContent
    = do xs <- many (bchars <|> bescchar <|> bbrace)
	 return $ concat xs
    where
    bchars
	= many1 (noneOf "{}\\")

    bbrace :: TclParser String
    bbrace
	= do
	  s <- between (char '{') (char '}') braceContent
	  return $ "{" ++ s ++ "}"

bescNL :: TclParser Char
bescNL
    = do
      try (string "\\\n")
      many (oneOf " \t\r")
      return ' '

bescCH :: TclParser String
bescCH
    = do
      char '\\'
      c <- anyChar
      return $ "\\" ++ [c]

bescchar :: TclParser String
bescchar
    = ( do
	bescNL
        return " "
      )
      <|>
      bescCH

noExtraChar :: String -> TclParser ()
noExtraChar na
    = notFollowedBy $ noneOf $ na ++ ws ++ nl

ws1 ::  TclParser ()
ws1 = do
      many1 (oneOf " \t\r" <|> bescNL)
      return ()

ws0 ::  TclParser ()
ws0 = option () ws1

nl1 :: TclParser ()
nl1 = ( do
	char '\n'
        option ' ' $ char '\r'
        return ()
      )
      <|>
      ( do
        char ';'
        return ()
      )

nl0 ::  TclParser ()
nl0 = option () nl1

-- ------------------------------------------------------------

p ps s = parse (do r <- ps
	           eof
	           return r
	       ) "" s
