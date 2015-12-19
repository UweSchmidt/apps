module Main where

import           PPL.Lexer
import           PPL.Parser
import           PPL.SemanticAnalysis

import           PPL.Assemble
import           PPL.CodeGeneration
import           PPL.OptimizeInstr

import           PPL.ShowAbstractSyntaxTree
import           PPL.ShowAttrTree
import           PPL.ShowCode

import           PPL.ShowCCode
import           PPL.ShowJavaCode

import           PPL.ControlUnit
import           PPL.Loader

import           Data.Char

import           System.Environment
import           System.IO

processFile :: ([Char] -> IO a) -> [Char] -> IO ()
processFile p fn
    = do
      h <- openFile fn ReadMode
      processHandle p h >> hClose h

processHandle :: ([Char] -> IO a) -> Handle -> IO a
processHandle p h
    = do
      c <- hGetContents h
      p c


scan, parse, check, gencode, optcode, ass, assopt, ccode,
 exec   :: String -> IO ()

jcode   :: String -> String -> IO()

scan    = putStr   . showLex                                                                 . lexer
parse   = putStr   . showAST                                                                 . parser . lexer
check   = putStr   . showAttrTree                                                . checkProg . parser . lexer
gencode = putStr   . showExecutable                             . codegeneration . checkProg . parser . lexer
optcode = putStr   . showExecutable             . optimizeInstr . codegeneration . checkProg . parser . lexer
ass     = putStr   . showExecutable1 . assemble                 . codegeneration . checkProg . parser . lexer
assopt  = putStr   . showExecutable1 . assemble . optimizeInstr . codegeneration . checkProg . parser . lexer
ccode   = putStr   . showCCode       . assemble . optimizeInstr . codegeneration . checkProg . parser . lexer
jcode c = putStr   . showJavaCode c  . assemble . optimizeInstr . codegeneration . checkProg . parser . lexer
exec    = execProg . loadExecutable  . assemble . optimizeInstr . codegeneration . checkProg . parser . lexer

jcode1          :: [String] -> String -> IO ()
jcode1 args     = jcode (getClassName args)

getClassName    :: [String] -> String
getClassName []
    = "Aout"

getClassName (fn:_)
    = takeWhile isAlphaNum fn

main    :: IO ()
main
    = do
      argl <- getArgs
      main1 argl


main1 :: [String] -> IO ()

main1 ("--scan":args)           = main2 scan args
main1 ("-s":args)               = main2 scan args
main1 ("--parse":args)          = main2 parse args
main1 ("-p":args)               = main2 parse args
main1 ("--check":args)          = main2 check args
main1 ("-c":args)               = main2 check args
main1 ("--gencode":args)        = main2 gencode args
main1 ("-g":args)               = main2 gencode args
main1 ("--optcode":args)        = main2 optcode args
main1 ("-o":args)               = main2 optcode args
main1 ("--ass":args)            = main2 ass args
main1 ("-a":args)               = main2 ass args
main1 ("--assopt":args)         = main2 assopt args
main1 ("-A":args)               = main2 assopt args
main1 ("--java":args)           = main2 (jcode1 args) args
main1 ("-j":args)               = main2 (jcode1 args) args
main1 ("--ccode":args)          = main2 ccode args
main1 ("-C":args)               = main2 ccode args
main1 ("--exec":args)           = main2 exec args
main1 ("-e":args)               = main2 exec args
main1 ("--help":_)              = usage
main1 ("-h":_)                  = usage

main1 args
    = do
      hPutStrLn
        stderr
        ("illegal arguments:" ++ concat (map (" "++) args))
      usage

usage :: IO ()
usage
    = hPutStrLn
        stderr ( "Usage:\n\tpplc Option [file]\n\n"
                 ++ "process a ppl program from file or from stdin\n"
                 ++ "version 0.1.0.2\n\n"
                 ++ "Options:\n"
                 ++ "\t-s, --scan\tlexical analysis\n"
                 ++ "\t-p, --parse\tsyntax analysis\n"
                 ++ "\t-c, --check\tsemantic analysis and type check\n"
                 ++ "\t-g, --gencode\tcode generation of assembler like code\n"
                 ++ "\t-o, --optcode\toptimize assembler like code\n"
                 ++ "\t-a, --ass\tcodegeneration with resolution of labels\n"
                 ++ "\t-A, --assopt\toptimized codegeneration with resolution of labels\n"
                 ++ "\t-j, --java\tgenerate java code\n"
                 ++ "\t-C, --ccode\tgenerate C code\n"
                 ++ "\t-e, --exec\tcompile and run\n"
                 ++ "\t-h, --help\tthis message\n"
               )

main2 :: (String -> IO ()) -> [String] -> IO ()
main2 fct []            = processHandle fct stdin
main2 fct (fn:_)        = processFile fct fn
