module A
where

import Photo2.CmdInterpreter
import Photo2.ArchiveTypes
import Photo2.FilePath
import Photo2.Config

m
    = do
      s1 <- cmdLoop emptyAppState
      return ()

