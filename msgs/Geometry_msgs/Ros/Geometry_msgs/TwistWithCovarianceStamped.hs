{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.TwistWithCovarianceStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.TwistWithCovariance as TwistWithCovariance
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data TwistWithCovarianceStamped = TwistWithCovarianceStamped { _header :: Header.Header
                                                             , _twist :: TwistWithCovariance.TwistWithCovariance
                                                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''TwistWithCovarianceStamped)

instance RosBinary TwistWithCovarianceStamped where
  put obj' = put (_header obj') *> put (_twist obj')
  get = TwistWithCovarianceStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader TwistWithCovarianceStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo TwistWithCovarianceStamped where
  sourceMD5 _ = "8927a1a12fb2607ceea095b2dc440a96"
  msgTypeName _ = "geometry_msgs/TwistWithCovarianceStamped"

instance D.Default TwistWithCovarianceStamped

