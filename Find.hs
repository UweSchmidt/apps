module Main
where

import Control.Monad

import Data.List hiding ( find )
import Data.Maybe

import Test.HUnit
    ( Test(..)
    , assertEqual
    , runTestTT
    , errors
    , failures
    )

import System
import System.Environment
import System.IO

import FindGrepSed

-- ------------------------------

actions	:: [(String, FilePath -> IO () )]
actions
    = [ ("-h",		usage	)
      , ("--help",	usage	)

      , ("findAscii",			findFiles asciiFiles		)
      , ("findBadFilenames",		findFiles badNames		)
      , ("findBinaryFiles",		findFiles binaryFiles		)
      , ("findBoringFiles",		findFiles boringFiles		)
      , ("findCvsFiles",		findFiles cvsFiles		)
      , ("findExecutables",		findFiles executableFiles	)
      , ("findExtensions",		findExts fileExtensions		)
      , ("findHtmlLatin1",		findFiles htmlLatin1Files	)
      , ("findHtmlUtf8",		findFiles htmlUtf8Files		)
      , ("findNoneAsciiProgs",		findFiles noneAsciiProgFiles	)
      , ("findTclLatin1",		findFiles tclLatin1Files	)
      , ("findUnknownFiles",		findFiles unknownFiles		)
      , ("findUppercaseImgFiles",	findFiles uppercaseImgFiles	)
      , ("findUnusedAlbumFiles",	processUnusedAlbumFiles printFiles	)

      , ("grepHtmlLatin1",		grepFiles isUmlaut htmlLatin1Files	)
      , ("grepHtmlUtf8",		grepFiles isUtf htmlUtf8Files		)
      , ("grepNoneAsciiProgs",		grepFiles isUmlaut noneAsciiProgFiles	)
      , ("grepTclLatin1",		grepFiles isUmlaut tclLatin1Files	)

      , ("sedHtmlLatin1",		sedFiles substXhtmlChars htmlLatin1Files	)
      , ("sedHtmlUtf8",			sedFiles substXhtmlUtf8Chars htmlUtf8Files	)
      , ("sedNoneAsciiProgs",		sedFiles substUmlauts noneAsciiProgFiles	)
      , ("sedTclLatin1",		sedFiles substLatin1Tcl tclLatin1Files		)

      , ("renameUppercaseImgFiles",	moveFiles uppercaseImgFiles	)

      , ("removeBoringFiles",		remFiles boringFiles	)
      , ("removeUnusedAlbumFiles",	processUnusedAlbumFiles removeFiles	)

      , ("hunitTest",			hunitTest	)
      ]

main :: IO ()
main
    = do
      al <- getArgs
      let fct = head . (++ ["-h"]) $ al
      let dir = head . (++ [""]) . drop 1 $ al 
      ( fromMaybe usage . lookup fct $ actions) dir
      return ()


usage	:: FilePath -> IO ()
usage _dir
    = do
      pn <- getProgName
      putStrLn ( "usage: " ++ pn ++ " [" ++ cmds ++ "] [dir]\n" )
    where
    cmds = foldl1 (\ x y -> x ++ " | " ++ y) . map fst $ actions

-- ------------------------------

-- a few hunit tests

findTests	:: Test
findTests
    = TestLabel "find tests" .
      TestList .
      map (uncurry findF)
      $
      [ ( noneAsciiProgFiles,
	  [ "Umlaut.tcl" ] )
      , ( AndExpr [progFiles, NotExpr noneAsciiProgFiles],
	  [ "Ascii.tcl" ] )
      , ( boringFiles,
	  [ "Umlaut.bak" ] )
      , ( htmlLatin1Files,
	  [ "Latin1.html", "Utf8.html" ] )
      , ( htmlUtf8Files,
	  [ "Utf8.html" ] )
      , ( tclLatin1Files,
	  [ "Umlaut.tcl" ] )
      ]
    where
    findF expr expected
	= TestCase $
	  do
	  res <- find "Tests" expr
	  assertEqual "find" (map (joinFile "Tests") expected) res

allTests	:: Test
allTests
    = TestList
      [ findTests
      ]

hunitTest	:: FilePath -> IO ()
hunitTest _
    = do
      c <- runTestTT allTests
      putStrLn $ show c
      let errs = errors c
	  fails = failures c
      System.exitWith (codeGet errs fails)

codeGet	:: Int -> Int -> ExitCode
codeGet errs fails
    | fails > 0       = ExitFailure 2
    | errs > 0        = ExitFailure 1
    | otherwise       = ExitSuccess

-- ------------------------------
