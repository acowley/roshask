{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.PoseWithCovarianceStamped where
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
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data PoseWithCovarianceStamped = PoseWithCovarianceStamped { _header :: Header.Header
                                                           , _pose :: PoseWithCovariance.PoseWithCovariance
                                                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''PoseWithCovarianceStamped)

instance RosBinary PoseWithCovarianceStamped where
  put obj' = put (_header obj') *> put (_pose obj')
  get = PoseWithCovarianceStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader PoseWithCovarianceStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo PoseWithCovarianceStamped where
  sourceMD5 _ = "953b798c0f514ff060a53a3498ce6246"
  msgTypeName _ = "geometry_msgs/PoseWithCovarianceStamped"

instance D.Default PoseWithCovarianceStamped

