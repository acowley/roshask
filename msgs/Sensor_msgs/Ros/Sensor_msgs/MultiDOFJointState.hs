{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.MultiDOFJointState where
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
import qualified Ros.Geometry_msgs.Transform as Transform
import qualified Ros.Geometry_msgs.Twist as Twist
import qualified Ros.Geometry_msgs.Wrench as Wrench
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data MultiDOFJointState = MultiDOFJointState { _header :: Header.Header
                                             , _joint_names :: [P.String]
                                             , _transforms :: V.Vector Transform.Transform
                                             , _twist :: V.Vector Twist.Twist
                                             , _wrench :: V.Vector Wrench.Wrench
                                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''MultiDOFJointState)

instance RosBinary MultiDOFJointState where
  put obj' = put (_header obj') *> putList (_joint_names obj') *> put (_transforms obj') *> put (_twist obj') *> put (_wrench obj')
  get = MultiDOFJointState <$> get <*> getList <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader MultiDOFJointState where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo MultiDOFJointState where
  sourceMD5 _ = "690f272f0640d2631c305eeb8301e59d"
  msgTypeName _ = "sensor_msgs/MultiDOFJointState"

instance D.Default MultiDOFJointState

