module Main
where

import Photo2.CmdInterpreter
import Photo2.ArchiveTypes

m	:: IO ()
m	= do
	  _ <- cmdLoop emptyAppState
	  return ()

main	:: IO ()
main	= m
