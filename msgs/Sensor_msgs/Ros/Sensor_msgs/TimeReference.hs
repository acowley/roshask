{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.TimeReference where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import Ros.Internal.RosTypes
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data TimeReference = TimeReference { _header :: Header.Header
                                   , _time_ref :: ROSTime
                                   , _source :: P.String
                                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''TimeReference)

instance RosBinary TimeReference where
  put obj' = put (_header obj') *> put (_time_ref obj') *> put (_source obj')
  get = TimeReference <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader TimeReference where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo TimeReference where
  sourceMD5 _ = "fded64a0265108ba86c3d38fb11c0c16"
  msgTypeName _ = "sensor_msgs/TimeReference"

instance D.Default TimeReference

