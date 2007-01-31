module ArgumentParser
    ( ArgParser
    , parseArgs
    , manyArgs
    , manyOpts
    , (<.>)
    , posArg
    , keywordArg
    , subProgArgs
    , subCommandArgs
    , longFlag
    , longFlags
    , longFlagOptions
    , longValOption
    , longOption
    , longNumOption
    , longDigitOption
    , shortFlag
    , shortFlags
    , shortValOption
    , shortOption
    , shortOptValOption
    , shortDigitOption
    , shortNumOption
    , shortLongFlag
    , shortLongOption
    , shortLongDigitOption
    , shortLongNumOption
    )
where

import Data.Maybe

import Text.ParserCombinators.Parsec

import System.IO
import System.Exit

-- ------------------------------

type ArgParser	= Parser [(String, String)]

parseArgs	:: ArgParser -> String -> [(String, String)] -> String -> [String] -> IO [(String, String)]
parseArgs p progName defaults usage argv
    | flagSet "help"
	= do
	  hPutStrLn stdout usage
	  exitWith ExitSuccess
    | flagSet "error"
	= do
	  hPutStrLn stderr (unlines errs)
	  hPutStrLn stderr usage
	  exitWith (ExitFailure (0-1))
    | otherwise
	= return res
    where
    res  = either fromErr id . parse pEof progName . concatMap (++ [argTerm]) $ argv
    errs = map ((progName ++ ": ") ++) . map snd . filter ((== "error") . fst)$ res
    flagSet n
	= not . all (=='0') . fromMaybe "" . lookup n $ res
    pEof = do
	   rs <- ( p <.> unknownArgs )
	   eof
	   return (rs ++ defaults)
    fromErr e
	= [("error", show $ e)]

-- ------------------------------

argTerm		:: Char
argTerm		= '\0'

eoArg		:: Parser Char
eoArg		= char argTerm

allButEoArg	:: Parser Char
allButEoArg	= noneOf [argTerm]

wholeArg1	:: Parser String
wholeArg1	= many1 allButEoArg

wholeArg	:: Parser String
wholeArg	= many  allButEoArg

-- ------------------------------

-- primitive argument parser

posArg :: String -> ArgParser
posArg name
    = oneArg $
      ( do
	res <- wholeArg1
	return [(name, res)]
      )

-- parse the value part of an option with value

argVal	:: String -> String -> Parser String -> ArgParser
argVal opt name parseVal
    = try ( do
	    val <- parseVal
	    return [(name, val)]
	  )
      <|>
      ( do
	val <- wholeArg
	return [("error", "illegal argument " ++ show (opt ++ val))]
      )

-- ------------------------------

-- argument parser combinators

-- sequence

(<.>)	:: ArgParser -> ArgParser -> ArgParser
p1 <.> p2
    = do
      r1 <- p1
      r2 <- p2
      return (r1 ++ r2)

-- ------------------------------

-- one complete argument

oneArg	:: ArgParser -> ArgParser
oneArg p
    = try $
      do
      res <- p
      eoArg
      return res

-- ------------------------------

manyArgs	:: ArgParser -> ArgParser
manyArgs p
    = do
      res <- many p
      return $ concat res

-- ------------------------------

fixedArg :: String -> String -> ArgParser
fixedArg name value
    = oneArg $
      ( do
	string value
	return [(name, value)]
      )

-- ------------------------------

keywordArg :: String -> [String] -> ArgParser
keywordArg name values
    = ( foldr1 (<|>) . map (fixedArg name) $ values )
      <|>
      return [("error",
	       "illegal " ++ name ++ ", one of " ++
	       (foldr1 (\ x y -> x ++ "," ++ y) . map show $ values) ++
	       " expected"
	      )
	     ]

-- ------------------------------

subProgArgs	:: [([String], ArgParser, ArgParser)] -> ArgParser
subProgArgs pt
    = subCommandArgs "operation"
      [ (c, manyOpts ops <.> aps)
	| (cs, ops, aps) <- pt
      , c <- cs
      ]

-- ------------------------------

subCommandArgs	:: String -> [(String, ArgParser)] -> ArgParser
subCommandArgs name cmds
    = foldr1 (<|>) ( zipWith subCmd cnames csubps )
      <|>
      return [("error",
	       "illegal argument, one of " ++
	       (foldr1 (\ x y -> x ++ "," ++ y) . map show $ cnames) ++ " expected")]

    where
    cnames = map fst cmds
    csubps = map snd cmds
    subCmd val sp
	= fixedArg name val <.> sp

-- ------------------------------

-- all given options until "--"

manyOpts	:: ArgParser -> ArgParser
manyOpts op
    = ( ( op <|> unknownOpt) <.> manyOpts op )
      <|>
      lastOpt
      <|>
      return []

-- ------------------------------

lastOpt		:: ArgParser
lastOpt
    = oneArg $
      ( do
	string "--"
	return []
      )

-- ------------------------------

unknownOpt		:: ArgParser
unknownOpt
    = oneArg $
      ( try ( do
	  string "--"
	  res <- wholeArg1
	  return [("error", "unknown option " ++ show ('-':'-':res))]
	)
	<|>
	try ( do
	  char '-'
	  c2 <- noneOf [argTerm,'-']
	  cs <- wholeArg
	  return [("error", "unknown option " ++ show ('-':c2:cs))]
	)
	<|>
	( do
	  char '-'
	  return [("error", "unknown option " ++ show "-")]
	)
      )

-- ------------------------------

unknownArgs		:: ArgParser
unknownArgs
    = ( unknownArg <.> unknownArgs )
      <|>
      return []

unknownArg		:: ArgParser
unknownArg
    = oneArg $
      ( ( do
	  res <- wholeArg1
	  return [("error", "illegal arg " ++ show res)]
	)
	<|>
	return []
      )

-- ------------------------------

-- long option parsers

-- long flag options like "--xxxxx"

longFlagOptions	:: [String] -> (String -> [(String, String)]) -> ArgParser
longFlagOptions names f
    = oneArg $
      ( do
	string "--"
	name <- foldr1 (<|>) . map string $ names
	return (f name)
      )

longFlags		:: [String] -> String -> ArgParser
longFlags names val	= longFlagOptions names (\ n -> [(n, val)])

longFlag		:: String -> String -> ArgParser
longFlag name val	= longFlagOptions [name] (const [(name, val)])

-- ------------------------------

-- long value options like "--xxxx=yyyy"

longValOption	:: String -> Parser String -> ArgParser
longValOption name parseVal
    = oneArg
      ( do
	string name'
	( try ( do
		char '='
		argVal name'' name parseVal
	      )
	  <|>
	  return [("error", show (name'' ++ "<value>") ++ " expected")] )
      )
    where
    name'  = '-':'-':name
    name'' = name' ++ "="

-- long value option with arbitary values

longOption		:: String -> ArgParser
longOption name 	= longValOption name wholeArg

-- long value option with numeric values

longNumOption		:: String -> ArgParser
longNumOption name	= longValOption name (many digit)

longDigitOption		:: String -> ArgParser
longDigitOption name	= longValOption name (count 1 digit)

-- ------------------------------

-- short flag options

shortFlags	:: String -> (Char -> [(String, String)]) -> ArgParser
shortFlags os f
    = oneArg $
      do
      char '-'
      res <- many1 (oneOf os )
      return $ concatMap f res

shortFlag		:: Char -> String -> String -> ArgParser
shortFlag c name val	= shortFlags [c] (const [(name,val)])

-- ------------------------------

-- short option with value like "-p xxx"

shortValOption :: Char -> String -> Parser String -> ArgParser
shortValOption c name parseVal
    = oneArg
      ( do
	string name'
	( try ( do
		eoArg
		argVal name'' name parseVal
	      )
	  <|>
	  return [("error", show (name'' ++ "<value>") ++ " expected")] )
      )
    where
    name'  = '-':c:[]
    name'' = name' ++ " "

shortOption		:: Char -> String -> ArgParser
shortOption c name	= shortValOption c name wholeArg

-- short option with value like "-t5" or "-t"

shortOptValOption :: Char -> String -> String -> Parser String -> ArgParser
shortOptValOption c name defVal parseVal
    = oneArg
      ( do
	string name'
	( try ( do
		argVal name' name (option defVal parseVal)
	      )
	  <|>
	  return [("error", show (name' ++ "[value]") ++ " expected")] )
      )
    where
    name'  = '-':c:[]

shortDigitOption 		:: Char -> String -> String -> ArgParser
shortDigitOption c name defVal	= shortOptValOption c name defVal (count 1 digit)

shortNumOption 			:: Char -> String -> String -> ArgParser
shortNumOption c name defVal	= shortOptValOption c name defVal (many1 digit)

-- ------------------------------

-- combination of short and long options

shortLongFlag			:: Char -> String -> String -> ArgParser
shortLongFlag c name val
    = longFlag name val
      <|>
      shortFlag c name val

shortLongOption			:: Char -> String -> ArgParser
shortLongOption c name
    = longOption name
      <|>
      shortOption c name

shortLongDigitOption		:: Char -> String -> String -> ArgParser
shortLongDigitOption c name defVal
    = longDigitOption name
      <|>
      shortDigitOption c name defVal

shortLongNumOption		:: Char -> String -> String -> ArgParser
shortLongNumOption c name defVal
    = longNumOption name
      <|>
      shortNumOption c name defVal

-- ------------------------------
