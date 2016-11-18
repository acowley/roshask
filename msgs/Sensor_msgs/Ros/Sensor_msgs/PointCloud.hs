{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.PointCloud where
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
import qualified Ros.Geometry_msgs.Point32 as Point32
import qualified Ros.Sensor_msgs.ChannelFloat32 as ChannelFloat32
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data PointCloud = PointCloud { _header :: Header.Header
                             , _points :: V.Vector Point32.Point32
                             , _channels :: [ChannelFloat32.ChannelFloat32]
                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''PointCloud)

instance RosBinary PointCloud where
  put obj' = put (_header obj') *> put (_points obj') *> putList (_channels obj')
  get = PointCloud <$> get <*> get <*> getList
  putMsg = putStampedMsg

instance HasHeader PointCloud where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo PointCloud where
  sourceMD5 _ = "d8e9c3f5afbdd8a130fd1d2763945fca"
  msgTypeName _ = "sensor_msgs/PointCloud"

instance D.Default PointCloud

