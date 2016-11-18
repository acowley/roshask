{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.LaserScan where
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
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data LaserScan = LaserScan { _header :: Header.Header
                           , _angle_min :: P.Float
                           , _angle_max :: P.Float
                           , _angle_increment :: P.Float
                           , _time_increment :: P.Float
                           , _scan_time :: P.Float
                           , _range_min :: P.Float
                           , _range_max :: P.Float
                           , _ranges :: V.Vector P.Float
                           , _intensities :: V.Vector P.Float
                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''LaserScan)

instance RosBinary LaserScan where
  put obj' = put (_header obj') *> put (_angle_min obj') *> put (_angle_max obj') *> put (_angle_increment obj') *> put (_time_increment obj') *> put (_scan_time obj') *> put (_range_min obj') *> put (_range_max obj') *> put (_ranges obj') *> put (_intensities obj')
  get = LaserScan <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader LaserScan where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo LaserScan where
  sourceMD5 _ = "90c7ef2dc6895d81024acba2ac42f369"
  msgTypeName _ = "sensor_msgs/LaserScan"

instance D.Default LaserScan

