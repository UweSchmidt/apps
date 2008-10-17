module Photo2.Config
where

import           Data.Atom
import qualified Data.Map as M
import           Data.Maybe

import           Photo2.ArchiveTypes
import           Photo2.FilePath

keyAlbumDir	:: Atom
keyAlbumDir	= newAtom "album-dir"

albumDir	:: Config -> FilePath
albumDir	= fromMaybe "album2" . M.lookup keyAlbumDir . confAttrs

albumPath	:: Path -> Config -> FilePath
albumPath p c	= albumDir c </> (joinPath p `addExtension` "xml")

