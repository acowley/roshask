{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.SetCameraInfoRequest where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Ros.Sensor_msgs.CameraInfo as CameraInfo
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data SetCameraInfoRequest = SetCameraInfoRequest { _camera_info :: CameraInfo.CameraInfo
                                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''SetCameraInfoRequest)

instance RosBinary SetCameraInfoRequest where
  put obj' = put (_camera_info obj')
  get = SetCameraInfoRequest <$> get

instance MsgInfo SetCameraInfoRequest where
  sourceMD5 _ = "ee34be01fdeee563d0d99cd594d5581d"
  msgTypeName _ = "sensor_msgs/SetCameraInfoRequest"

instance D.Default SetCameraInfoRequest

instance SrvInfo SetCameraInfoRequest where
  srvMD5 _ = "bef1df590ed75ed1f393692395e15482"
  srvTypeName _ = "sensor_msgs/SetCameraInfo"

