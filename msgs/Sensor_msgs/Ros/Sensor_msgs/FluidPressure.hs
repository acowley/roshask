{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.FluidPressure where
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

data FluidPressure = FluidPressure { _header :: Header.Header
                                   , _fluid_pressure :: P.Double
                                   , _variance :: P.Double
                                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''FluidPressure)

instance RosBinary FluidPressure where
  put obj' = put (_header obj') *> put (_fluid_pressure obj') *> put (_variance obj')
  get = FluidPressure <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader FluidPressure where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo FluidPressure where
  sourceMD5 _ = "804dc5cea1c5306d6a2eb80b9833befe"
  msgTypeName _ = "sensor_msgs/FluidPressure"

instance D.Default FluidPressure

