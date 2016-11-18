{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.OccupancyGrid where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Data.Int as Int
import qualified Data.Vector.Storable as V
import qualified Ros.Nav_msgs.MapMetaData as MapMetaData
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data OccupancyGrid = OccupancyGrid { _header :: Header.Header
                                   , _info :: MapMetaData.MapMetaData
                                   , __data :: V.Vector Int.Int8
                                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''OccupancyGrid)

instance RosBinary OccupancyGrid where
  put obj' = put (_header obj') *> put (_info obj') *> put (__data obj')
  get = OccupancyGrid <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader OccupancyGrid where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo OccupancyGrid where
  sourceMD5 _ = "3381f2d731d4076ec5c71b0759edbe4e"
  msgTypeName _ = "nav_msgs/OccupancyGrid"

instance D.Default OccupancyGrid

