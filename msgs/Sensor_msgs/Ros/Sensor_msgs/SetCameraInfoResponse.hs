{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.SetCameraInfoResponse where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Data.Word as Word
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data SetCameraInfoResponse = SetCameraInfoResponse { _success :: P.Bool
                                                   , _status_message :: P.String
                                                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''SetCameraInfoResponse)

instance RosBinary SetCameraInfoResponse where
  put obj' = put (_success obj') *> put (_status_message obj')
  get = SetCameraInfoResponse <$> get <*> get

instance MsgInfo SetCameraInfoResponse where
  sourceMD5 _ = "2ec6f3eff0161f4257b808b12bc830c2"
  msgTypeName _ = "sensor_msgs/SetCameraInfoResponse"

instance D.Default SetCameraInfoResponse

instance SrvInfo SetCameraInfoResponse where
  srvMD5 _ = "bef1df590ed75ed1f393692395e15482"
  srvTypeName _ = "sensor_msgs/SetCameraInfo"

