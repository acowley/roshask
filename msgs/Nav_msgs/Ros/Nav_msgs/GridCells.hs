{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GridCells where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Data.Vector.Storable as V
import qualified Ros.Geometry_msgs.Point as Point
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GridCells = GridCells { _header :: Header.Header
                           , _cell_width :: P.Float
                           , _cell_height :: P.Float
                           , _cells :: V.Vector Point.Point
                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GridCells)

instance RosBinary GridCells where
  put obj' = put (_header obj') *> put (_cell_width obj') *> put (_cell_height obj') *> put (_cells obj')
  get = GridCells <$> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader GridCells where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo GridCells where
  sourceMD5 _ = "b9e4f5df6d28e272ebde00a3994830f5"
  msgTypeName _ = "nav_msgs/GridCells"

instance D.Default GridCells

