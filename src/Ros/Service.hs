module Ros.Service (callService) where
import System.Environment(getEnvironment)

import Ros.Node.RosTcp(callServiceWithMaster)
import Ros.Internal.RosTypes
import Ros.Internal.RosBinary
import Ros.Internal.Msg.SrvInfo
import Ros.Service.ServiceTypes


--type NotOkError = String

callService :: (RosBinary a, SrvInfo a, RosBinary b, SrvInfo b) => ServiceName -> a -> IO (Either ServiceResponseExcept b)
callService name req =
  do
    env <- getEnvironment
    let getConfig' var def = maybe def id $ lookup var env
        master = getConfig' "ROS_MASTER_URI" "http://localhost:11311"
    callServiceWithMaster master name req
