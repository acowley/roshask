{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.Temperature where
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

data Temperature = Temperature { _header :: Header.Header
                               , _temperature :: P.Double
                               , _variance :: P.Double
                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Temperature)

instance RosBinary Temperature where
  put obj' = put (_header obj') *> put (_temperature obj') *> put (_variance obj')
  get = Temperature <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader Temperature where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo Temperature where
  sourceMD5 _ = "ff71b307acdbe7c871a5a6d7ed359100"
  msgTypeName _ = "sensor_msgs/Temperature"

instance D.Default Temperature

