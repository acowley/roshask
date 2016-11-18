{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.Odometry where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.PoseWithCovariance as PoseWithCovariance
import qualified Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Odometry = Odometry { _header :: Header.Header
                         , _child_frame_id :: P.String
                         , _pose :: PoseWithCovariance.PoseWithCovariance
                         , _twist :: TwistWithCovariance.TwistWithCovariance
                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Odometry)

instance RosBinary Odometry where
  put obj' = put (_header obj') *> put (_child_frame_id obj') *> put (_pose obj') *> put (_twist obj')
  get = Odometry <$> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader Odometry where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo Odometry where
  sourceMD5 _ = "cd5e73d190d741a2f92e81eda573aca7"
  msgTypeName _ = "nav_msgs/Odometry"

instance D.Default Odometry

