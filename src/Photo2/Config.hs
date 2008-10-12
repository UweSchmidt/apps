module Photo2.Config
where

import qualified Data.Map as M
import           Data.Maybe

import           Photo2.ArchiveTypes
import           Photo2.FilePath

albumDir	:: Config -> FilePath
albumDir	= fromMaybe "album2" . M.lookup "album-dir" . confAttrs

albumPath	:: Path -> Config -> FilePath
albumPath p c	= albumDir c </> (joinPath p `addExtension` "xml")

