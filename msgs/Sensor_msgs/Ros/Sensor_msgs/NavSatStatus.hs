{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.NavSatStatus where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Int as Int
import qualified Data.Word as Word
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data NavSatStatus = NavSatStatus { _status :: Int.Int8
                                 , _service :: Word.Word16
                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''NavSatStatus)

instance RosBinary NavSatStatus where
  put obj' = put (_status obj') *> put (_service obj')
  get = NavSatStatus <$> get <*> get

instance Storable NavSatStatus where
  sizeOf _ = sizeOf (P.undefined::Int.Int8) +
             sizeOf (P.undefined::Word.Word16)
  alignment _ = 8
  peek = SM.runStorable (NavSatStatus <$> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_status obj') *> SM.poke (_service obj')

instance MsgInfo NavSatStatus where
  sourceMD5 _ = "331cdbddfa4bc96ffc3b9ad98900a54c"
  msgTypeName _ = "sensor_msgs/NavSatStatus"

instance D.Default NavSatStatus

status_no_fix :: Int.Int8
status_no_fix = -1

status_fix :: Int.Int8
status_fix = 0

status_sbas_fix :: Int.Int8
status_sbas_fix = 1

status_gbas_fix :: Int.Int8
status_gbas_fix = 2

service_gps :: Word.Word16
service_gps = 1

service_glonass :: Word.Word16
service_glonass = 2

service_compass :: Word.Word16
service_compass = 4

service_galileo :: Word.Word16
service_galileo = 8

