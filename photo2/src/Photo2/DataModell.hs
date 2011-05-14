module Photo2.DataModell
where

import Control.Monad hiding (when)

import           Data.AssocList
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Maybe
import           Data.Tree.NTree.TypeDefs(NTree(..))

import           Text.XML.HXT.Arrow

import           Photo2.ArchiveTypes

-- ------------------------------------------------------------

data AlbumDir   = AlbumDir { albumTree    :: AlbumTree'
                           , albumEntries :: AlbumEntries
                           , albumInode   :: INode
                           }

type AlbumTree' = NTree (Name, INode)

type AlbumEntries
                = IntMap AlbumEntry
                -- = Map INode AlbumEntry

type INode      = Int

-- ------------------------------------------------------------

emptyAlbumTree' :: AlbumTree'
emptyAlbumTree' = NTree (emptyName, 0) []

emptyAlbumDir   :: AlbumDir
emptyAlbumDir   = AlbumDir { albumTree    = emptyAlbumTree'
                           , albumEntries = IM.empty
                           , albumInode   = 1
                           }

-- ------------------------------------------------------------

getTreeByPath   :: Path -> LA AlbumTree AlbumTree
getTreeByPath p
    | null p    = none
    | null p'   = nodeMatch `guards` this
    | otherwise = nodeMatch `guards` (getChildren >>> getTreeByPath p')
    where
    (n' : p') = p
    nodeMatch = hasPicName n'

getDescByPath   :: Path -> LA AlbumTree AlbumTree
getDescByPath p
    | null p    = this
    | otherwise = nodeMatch `guards` (getChildren >>> getDescByPath p')
    where
    (n' : p') = p
    nodeMatch = hasPicName n'

processTreeByPath       :: Path -> LA AlbumTree AlbumTree ->  LA AlbumTree AlbumTree
processTreeByPath p pa
    | null p    = this
    | null p'   = pa `when` hasPicName n'
    | otherwise = processChildren (processTreeByPath p' pa)
    where
    (n' : p') = p

hasPicName      :: Name -> LA AlbumTree AlbumTree
hasPicName n
    = (getNode >>> arr picId >>> isA (==n)) `guards` this

mkPic   :: Pic -> LA AlbumTree AlbumTree
mkPic pic
    = constA $ NTree pic []

getAlbumEntry   :: Path -> LA AlbumTree AlbumEntry
getAlbumEntry p
    | null p    = none
    | otherwise = getDescByPath p'
                  >>>
                  hasPicName n'
                  >>>
                  getNode
                  >>>
                  arr (\ x -> (p, x))
    where
    n' = last p
    p' = init p

getAlbumPaths   :: Path -> LA AlbumTree Path
getAlbumPaths p
    = getDescByPath p
      >>>
      getPaths (reverse p)
      >>>
      arr reverse
    where
    getPaths :: Path -> LA AlbumTree Path
    getPaths p'
        = getPaths' $< (getNode >>> arr picId)
          where
          getPaths' :: Name -> LA AlbumTree Path
          getPaths' n'
              = constA p''
                <+>
                ( getChildren >>> getPaths p'' )
                where
                p'' = n' : p'

addAlbumEntry   :: AlbumEntry -> LA AlbumTree AlbumTree
addAlbumEntry (p, pic)
    = insert p
    where
    n = picId pic
    insert      :: Path -> LA AlbumTree AlbumTree
    insert p
        | null p        = setNode pic `when` hasPicName n               -- entry already in tree
        | null p'       = replaceChildren (getChildren <+> mkPic pic)   -- append a new leave to the children
                          `when`
                          hasPicName n'
        | otherwise     = processChildren (insert p')                   -- descend into tree
                          `when`
                          hasPicName n'
        where
        (n'  : p' ) = p

removeAlbumEntry        :: Path -> LA AlbumTree AlbumTree
removeAlbumEntry p
    = processTreeByPath p none

-- ------------------------------------------------------------

getPic  :: Path -> AlbumTree -> Pic
getPic p at
    = fromMaybe emptyPic
      . listToMaybe
      . map snd
      . runLA (getAlbumEntry p)
      $ at

setPic  :: Path -> Pic -> AlbumTree -> AlbumTree
setPic path pic at
    = head . runLA (addAlbumEntry (path, pic)) $ at

-- ------------------------------------------------------------
-- tree arrows

getTreeByPath'  :: Path -> LA AlbumTree' AlbumTree'
getTreeByPath' p
    | null p    = none
    | null p'   = nodeMatch `guards` this
    | otherwise = nodeMatch `guards` (getChildren >>> getTreeByPath' p')
    where
    (n':p') = p
    nodeMatch = getNode >>> arr fst >>> isA (==n')

processTreeByPath'      :: Path -> LA AlbumTree' AlbumTree' ->  LA AlbumTree' AlbumTree'
processTreeByPath' p pa
    | null p    = this
    | null p'   = pa `when` nodeMatch
    | otherwise = processChildren (processTreeByPath' p' pa)
    where
    (n':p') = p
    nodeMatch = getNode >>> arr fst >>> isA (==n')

getINode        :: Path -> LA AlbumTree' INode
getINode p      = getTreeByPath' p >>> getNode >>> arr snd

getAllINodes    :: Path -> LA AlbumTree' INode
getAllINodes p  = getTreeByPath' p >>> multi (getNode >>> arr snd)

removeTreeByPath'       :: Path -> LA AlbumTree' AlbumTree'
removeTreeByPath' p     = processTreeByPath' p none

mkPath' :: Path -> LA AlbumTree' AlbumTree'
mkPath' p
    | null p    = this
    | otherwise = undefined
    where
    (n':p') = p

-- ------------------------------------------------------------


getPic' :: Path -> AlbumDir -> Maybe Pic
getPic' p ad
    = listToMaybe
      . map snd
      . catMaybes
      . map (flip IM.lookup (albumEntries ad))
      . runLA (getINode p)
      . albumTree
      $ ad

setPic' :: Path -> Maybe Pic -> AlbumDir -> AlbumDir
setPic' p Nothing ad
    = remPic' p ad

setPic' p (Just pic) ad
    = undefined
    where
    ae = (p, pic)

remPic' :: Path -> AlbumDir -> AlbumDir
remPic' p ad
    = ad { albumEntries = ae'
         , albumTree    = at'
         }
    where
    at     = albumTree ad
    at'    = head . runLA (removeTreeByPath' p) $ at
    inodes =        runLA (getAllINodes p)     $ at
    ae'    = foldl (flip IM.delete) (albumEntries ad) $ inodes

-- ------------------------------------------------------------