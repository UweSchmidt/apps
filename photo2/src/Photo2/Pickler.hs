module Photo2
where

import           Data.Map (Map)
import qualified Data.Map as M

data Config     = Config { confAttrs   :: Attrs
                         , confLayouts :: Layouts
                         , confSizes   :: Sizes
                         }

type Attrs      = Map Name Value
type Name       = String
type Value      = String

type Layouts    = Map Name Layout
data Layout     = Layout { layoutType  :: Value
                         , layoutAttrs :: Attrs
                         , layoutPages :: Pages
                         }

type Sizes      = [Size]
type Size       = Size { sizeDir    :: String
                       , sizeGeo    :: Geo
                       , sizeAspect :: AspectRatio
                       }

data AspectRation
                = Fix | Pad
data Geo        = Geo { geoWidth  :: Int
                      , geoHeight :: Int
                      }

