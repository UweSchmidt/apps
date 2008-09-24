module Main
where

import Photo2.CmdInterpreter
import Photo2.ArchiveTypes
-- import Photo2.FilePath
-- import Photo2.Config

m	:: IO ()
m	= do
	  cmdLoop emptyAppState
	  return ()

main	:: IO ()
main	= m
