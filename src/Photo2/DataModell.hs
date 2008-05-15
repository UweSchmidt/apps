module Photo2.DataModell
where

import Control.Monad hiding (when)

import           Data.AssocList
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import		 Data.Maybe
import           Data.Tree.NTree.TypeDefs(NTree(..))

import           Text.XML.HXT.Arrow

import           Photo2.ArchiveTypes

-- ------------------------------------------------------------

data AlbumDir	= AlbumDir { albumTree'    :: AlbumTree
			   , albumEntries' :: AlbumEntries
			   , albumInode'   :: INode
			   }

type AlbumTree	= NTree (Name, INode)

type AlbumEntries
		= IntMap AlbumEntry
		-- = Map INode AlbumEntry

data AlbumEntry	= AlbumEntry { aePath'  :: Path
			     , aeDescr' :: Pic
			     }

type Path	= [Name]
type INode	= Int

type Options	= AssocList Name Value

-- ------------------------------------------------------------

emptyAlbumTree	:: AlbumTree
emptyAlbumTree	= NTree ("",0) []

emptyAlbumDir	:: AlbumDir
emptyAlbumDir	= AlbumDir { albumTree'    = emptyAlbumTree
			   , albumEntries' = IM.empty
			   , albumInode'   = 1
			   }

-- ------------------------------------------------------------
-- tree arrows

getTreeByPath	:: Path -> LA AlbumTree AlbumTree
getTreeByPath p
    | null p	= none
    | null p'	= nodeMatch `guards` this
    | otherwise = nodeMatch `guards` (getChildren >>> getTreeByPath p')
    where
    (n':p') = p
    nodeMatch = getNode >>> arr fst >>> isA (==n')

processTreeByPath	:: Path -> LA AlbumTree AlbumTree ->  LA AlbumTree AlbumTree
processTreeByPath p pa
    | null p	= this
    | null p'	= pa `when` nodeMatch
    | otherwise	= processChildren (processTreeByPath p' pa)
    where
    (n':p') = p
    nodeMatch = getNode >>> arr fst >>> isA (==n')

getINode	:: Path -> LA AlbumTree INode
getINode p	= getTreeByPath p >>> getNode >>> arr snd

getAllINodes	:: Path -> LA AlbumTree INode
getAllINodes p	= getTreeByPath p >>> multi (getNode >>> arr snd)

removeTreeByPath	:: Path -> LA AlbumTree AlbumTree
removeTreeByPath p	= processTreeByPath p none

-- ------------------------------------------------------------


getPic	:: Path -> AlbumDir -> Maybe Pic
getPic p ad
    = listToMaybe
      . map aeDescr'
      . catMaybes
      . map (flip IM.lookup (albumEntries' ad))
      . runLA (getINode p)
      . albumTree'
      $ ad

setPic	:: Path -> Maybe Pic -> AlbumDir -> AlbumDir
setPic p Nothing ad
    = remPic p ad

setPic p (Just pic) ad
    = undefined
    where
    ae = AlbumEntry { aePath'  = p
		    , aeDescr' = pic
		    }

remPic	:: Path -> AlbumDir -> AlbumDir
remPic p ad
    = ad { albumEntries' = ae'
	 , albumTree'    = at'
	 }
    where
    at     = albumTree' ad
    at'    = head . runLA (removeTreeByPath p) $ at
    inodes =        runLA (getAllINodes p)     $ at
    ae'    = foldl (flip IM.delete) (albumEntries' ad) $ inodes

-- ------------------------------------------------------------