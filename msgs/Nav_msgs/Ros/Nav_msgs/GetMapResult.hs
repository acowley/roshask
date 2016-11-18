{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetMapResult where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Ros.Nav_msgs.OccupancyGrid as OccupancyGrid
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GetMapResult = GetMapResult { _map :: OccupancyGrid.OccupancyGrid
                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GetMapResult)

instance RosBinary GetMapResult where
  put obj' = put (_map obj')
  get = GetMapResult <$> get

instance MsgInfo GetMapResult where
  sourceMD5 _ = "6cdd0a18e0aff5b0a3ca2326a89b54ff"
  msgTypeName _ = "nav_msgs/GetMapResult"

instance D.Default GetMapResult

