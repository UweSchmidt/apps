module Main
where

import System.Console.ArgumentParser

import Text.ParserCombinators.Parsec

import System.Environment
import System.IO
import System.Exit

main	:: IO ()
main
    = do
      pn   <- getProgName
      argv <- getArgs
      al  <- parseArgs subCmdParser pn defaultArgs (usage pn) argv
      putStrLn $ show al
      exitWith ExitSuccess

usage	:: String -> String
usage pn
    = unlines
      [ "usage: " ++ pn ++ " [GLOBAL-OPION]... <CMD> [OPTION]... [SUBALBUM] [ACHIVE]"
      , "Album management"
      , ""
      , "Global options:"
      , "\t-h, -?,\t--help\t\tdisplay this text and exit"
      , "\t-t[N],\t--trace=N\ttrace level (N=1-4), default: 1"
      , ""
      , "Commands:"
      , "\tlist-albums\t\tlist all album names in archive"
      , "\tlist-pictures\t\tlist all pictures in archive"
      , "\ttlist-all\t\tlist all albums and pictures"
      , "\tlist-attribute\t\tlist attributes of attribute given by \"--attribute\" option"
      , "\tupdate\t\t\tupdate the albums: check new pictures, copies, attributes and HTML pages"
      , "\trename\t\t\trename all pictures in an album into a standard number format"
      , ""
      , "Options:"
      , "  -x,\t--exec\t\t\texecute commands (all modifiying commands)"
      , "\t--no-exec, --dry-run\tdry run, do not execute modifying commands"
      , "\t\t\t\tDEFAULT (all modifying commands)"
      , "\t--force-orig\t\tforce copying pictures from original dir (update)"
      , "\t--force-attr\t\tforce recomputation of picture attributes (update)"
      , "\t--force-copy\t\tforce generation of picture copies of required sizes (update)"
      , "\t--force-html\t\tforce generation of HTML pages (update, rename)"
      ]

-- ------------------------------

subCmdParser		:: ArgParser
subCmdParser
    = manyOpts commonOptions
      <.>
      subProgArgs
      [ ( ["list-albums", "list-pictures", "list-all"]
	, commonOptions
	, albumArchive
	)
      , ( ["list-attr"]
	, attrArg <|> commonOptions
	, albumArchive
	)
      , ( ["update", "rename"]
	, updateArgs <|> commonOptionsDr
	, albumArchive
	)
      ]

commonOptions		:: ArgParser
commonOptions	= helpArg <|> traceArg

commonOptionsDr		:: ArgParser
commonOptionsDr	= dryRunArg <|> commonOptions

albumArchive		:: ArgParser
albumArchive	= subAlbum <.> archive

subAlbum		:: ArgParser
subAlbum	= option [] (posArg "subalbum")

archive		:: ArgParser
archive		= option [] (posArg "source")

importdir		:: ArgParser
importdir	= option [] (posArg "import-dir")

argParser		:: ArgParser
argParser = operation
	    <.>
	    optionArgs
	    <.>
	    option [] (posArg "source")
	    <.>
	    option [] (posArg "subalbum")
	    <.>
	    option [] (posArg "import-dir")

defaultArgs		:: [(String, String)]
defaultArgs = [ ("dry-run", "1")
	      , ("force-orig", "0")
	      , ("force-attr", "0")
	      , ("force-copy", "0")
	      , ("force-html", "0")
	      , ("source", "archive.xml")
	      , ("help", "0")
	      ]


operation		:: ArgParser
operation
    = keywordArg "operation"
      [ "list-albums"
      , "list-pictures"
      , "list-all"
      , "list-attr"
      , "update"
      , "rename"
      , "new-album"
      , "new-pictures"
      ]

optionArgs		:: ArgParser
optionArgs
    = manyOpts
      ( dryRunArg	<|>
	traceArg	<|>
	helpArg		<|>
	proxyArg	<|>
	updateArgs
      )

-- standard options

curlFlag		:: ArgParser
curlFlag	= longFlag 	      "use-curl" "1"

curlOptArg		:: ArgParser
curlOptArg	= longOption          "options-curl"

encodingArg		:: ArgParser
encodingArg	= shortLongOption 'e' "encoding"

outEncoding		:: ArgParser
outEncoding	= shortLongOption 'c' "output-encoding"

proxyArg		:: ArgParser
proxyArg	= shortLongOption 'p' "proxy"

helpArg		:: ArgParser
helpArg		= shortLongFlag   'h' "help" "1"
		  <|>
		  shortFlag       '?' "help" "1"

traceArg		:: ArgParser
traceArg	= shortLongDigitOption 't' "trace" "1"

attrArg		:: ArgParser
attrArg		= longOption "attribute"

updateArgs		:: ArgParser
updateArgs	= longFlags
		  [ "force-orig"
		  , "force-attr"
		  , "force-copy"
		  , "force-html"
		  , "normalize"
		  , "sort"
		  , "remove-errors"
		  , "remove-old-attr"
		  ] "1"

dryRunArg		:: ArgParser
dryRunArg	= longFlagOptions
		  [ "no-exec"
		  , "exec"
		  ] (\ s -> [ ("dry-run", if s == "exec" then "0" else "1") ] )
		  <|>
		  shortFlag 'x' "dry-run" "0"

-- ------------------------------
