{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.SetMapRequest where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Ros.Geometry_msgs.PoseWithCovarianceStamped as PoseWithCovarianceStamped
import qualified Ros.Nav_msgs.OccupancyGrid as OccupancyGrid
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data SetMapRequest = SetMapRequest { _map :: OccupancyGrid.OccupancyGrid
                                   , _initial_pose :: PoseWithCovarianceStamped.PoseWithCovarianceStamped
                                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''SetMapRequest)

instance RosBinary SetMapRequest where
  put obj' = put (_map obj') *> put (_initial_pose obj')
  get = SetMapRequest <$> get <*> get

instance MsgInfo SetMapRequest where
  sourceMD5 _ = "91149a20d7be299b87c340df8cc94fd4"
  msgTypeName _ = "nav_msgs/SetMapRequest"

instance D.Default SetMapRequest

instance SrvInfo SetMapRequest where
  srvMD5 _ = "c36922319011e63ed7784112ad4fdd32"
  srvTypeName _ = "nav_msgs/SetMap"

