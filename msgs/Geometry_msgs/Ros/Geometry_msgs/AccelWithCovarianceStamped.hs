{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.AccelWithCovarianceStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.AccelWithCovariance as AccelWithCovariance
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data AccelWithCovarianceStamped = AccelWithCovarianceStamped { _header :: Header.Header
                                                             , _accel :: AccelWithCovariance.AccelWithCovariance
                                                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''AccelWithCovarianceStamped)

instance RosBinary AccelWithCovarianceStamped where
  put obj' = put (_header obj') *> put (_accel obj')
  get = AccelWithCovarianceStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader AccelWithCovarianceStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo AccelWithCovarianceStamped where
  sourceMD5 _ = "96adb295225031ec8d57fb4251b0a886"
  msgTypeName _ = "geometry_msgs/AccelWithCovarianceStamped"

instance D.Default AccelWithCovarianceStamped

