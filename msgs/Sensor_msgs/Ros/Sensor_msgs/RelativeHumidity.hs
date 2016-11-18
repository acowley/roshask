{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.RelativeHumidity where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data RelativeHumidity = RelativeHumidity { _header :: Header.Header
                                         , _relative_humidity :: P.Double
                                         , _variance :: P.Double
                                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''RelativeHumidity)

instance RosBinary RelativeHumidity where
  put obj' = put (_header obj') *> put (_relative_humidity obj') *> put (_variance obj')
  get = RelativeHumidity <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader RelativeHumidity where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo RelativeHumidity where
  sourceMD5 _ = "8730015b05955b7e992ce29a2678d90f"
  msgTypeName _ = "sensor_msgs/RelativeHumidity"

instance D.Default RelativeHumidity

