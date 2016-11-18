{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetMapResponse where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Ros.Nav_msgs.OccupancyGrid as OccupancyGrid
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GetMapResponse = GetMapResponse { _map :: OccupancyGrid.OccupancyGrid
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GetMapResponse)

instance RosBinary GetMapResponse where
  put obj' = put (_map obj')
  get = GetMapResponse <$> get

instance MsgInfo GetMapResponse where
  sourceMD5 _ = "6cdd0a18e0aff5b0a3ca2326a89b54ff"
  msgTypeName _ = "nav_msgs/GetMapResponse"

instance D.Default GetMapResponse

instance SrvInfo GetMapResponse where
  srvMD5 _ = "6cdd0a18e0aff5b0a3ca2326a89b54ff"
  srvTypeName _ = "nav_msgs/GetMap"

