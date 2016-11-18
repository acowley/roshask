{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.Illuminance where
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

data Illuminance = Illuminance { _header :: Header.Header
                               , _illuminance :: P.Double
                               , _variance :: P.Double
                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Illuminance)

instance RosBinary Illuminance where
  put obj' = put (_header obj') *> put (_illuminance obj') *> put (_variance obj')
  get = Illuminance <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader Illuminance where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo Illuminance where
  sourceMD5 _ = "8cf5febb0952fca9d650c3d11a81a188"
  msgTypeName _ = "sensor_msgs/Illuminance"

instance D.Default Illuminance

