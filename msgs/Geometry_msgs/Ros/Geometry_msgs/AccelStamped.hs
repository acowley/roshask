{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.AccelStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Accel as Accel
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data AccelStamped = AccelStamped { _header :: Header.Header
                                 , _accel :: Accel.Accel
                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''AccelStamped)

instance RosBinary AccelStamped where
  put obj' = put (_header obj') *> put (_accel obj')
  get = AccelStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader AccelStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo AccelStamped where
  sourceMD5 _ = "d8a98a5d81351b6eb0578c78557e7659"
  msgTypeName _ = "geometry_msgs/AccelStamped"

instance D.Default AccelStamped

