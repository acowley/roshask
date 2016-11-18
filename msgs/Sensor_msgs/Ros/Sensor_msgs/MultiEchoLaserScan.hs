{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.MultiEchoLaserScan where
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
import qualified Ros.Sensor_msgs.LaserEcho as LaserEcho
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data MultiEchoLaserScan = MultiEchoLaserScan { _header :: Header.Header
                                             , _angle_min :: P.Float
                                             , _angle_max :: P.Float
                                             , _angle_increment :: P.Float
                                             , _time_increment :: P.Float
                                             , _scan_time :: P.Float
                                             , _range_min :: P.Float
                                             , _range_max :: P.Float
                                             , _ranges :: [LaserEcho.LaserEcho]
                                             , _intensities :: [LaserEcho.LaserEcho]
                                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''MultiEchoLaserScan)

instance RosBinary MultiEchoLaserScan where
  put obj' = put (_header obj') *> put (_angle_min obj') *> put (_angle_max obj') *> put (_angle_increment obj') *> put (_time_increment obj') *> put (_scan_time obj') *> put (_range_min obj') *> put (_range_max obj') *> putList (_ranges obj') *> putList (_intensities obj')
  get = MultiEchoLaserScan <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> getList <*> getList
  putMsg = putStampedMsg

instance HasHeader MultiEchoLaserScan where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo MultiEchoLaserScan where
  sourceMD5 _ = "6fefb0c6da89d7c8abe4b339f5c2f8fb"
  msgTypeName _ = "sensor_msgs/MultiEchoLaserScan"

instance D.Default MultiEchoLaserScan

