{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.Imu where
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
import qualified Ros.Geometry_msgs.Quaternion as Quaternion
import qualified Ros.Geometry_msgs.Vector3 as Vector3
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Imu = Imu { _header :: Header.Header
               , _orientation :: Quaternion.Quaternion
               , _orientation_covariance :: V.Vector P.Double
               , _angular_velocity :: Vector3.Vector3
               , _angular_velocity_covariance :: V.Vector P.Double
               , _linear_acceleration :: Vector3.Vector3
               , _linear_acceleration_covariance :: V.Vector P.Double
               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Imu)

instance RosBinary Imu where
  put obj' = put (_header obj') *> put (_orientation obj') *> put (_orientation_covariance obj') *> put (_angular_velocity obj') *> put (_angular_velocity_covariance obj') *> put (_linear_acceleration obj') *> put (_linear_acceleration_covariance obj')
  get = Imu <$> get <*> get <*> get <*> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader Imu where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo Imu where
  sourceMD5 _ = "6a62c6daae103f4ff57a132d6f95cec2"
  msgTypeName _ = "sensor_msgs/Imu"

instance D.Default Imu

