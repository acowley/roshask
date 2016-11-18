{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.BatteryState where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Data.Vector.Storable as V
import qualified Data.Word as Word
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data BatteryState = BatteryState { _header :: Header.Header
                                 , _voltage :: P.Float
                                 , _current :: P.Float
                                 , _charge :: P.Float
                                 , _capacity :: P.Float
                                 , _design_capacity :: P.Float
                                 , _percentage :: P.Float
                                 , _power_supply_status :: Word.Word8
                                 , _power_supply_health :: Word.Word8
                                 , _power_supply_technology :: Word.Word8
                                 , _present :: P.Bool
                                 , _cell_voltage :: V.Vector P.Float
                                 , _location :: P.String
                                 , _serial_number :: P.String
                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''BatteryState)

instance RosBinary BatteryState where
  put obj' = put (_header obj') *> put (_voltage obj') *> put (_current obj') *> put (_charge obj') *> put (_capacity obj') *> put (_design_capacity obj') *> put (_percentage obj') *> put (_power_supply_status obj') *> put (_power_supply_health obj') *> put (_power_supply_technology obj') *> put (_present obj') *> put (_cell_voltage obj') *> put (_location obj') *> put (_serial_number obj')
  get = BatteryState <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader BatteryState where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo BatteryState where
  sourceMD5 _ = "476f837fa6771f6e16e3bf4ef96f8770"
  msgTypeName _ = "sensor_msgs/BatteryState"

instance D.Default BatteryState

power_supply_status_unknown :: Word.Word8
power_supply_status_unknown = 0

power_supply_status_charging :: Word.Word8
power_supply_status_charging = 1

power_supply_status_discharging :: Word.Word8
power_supply_status_discharging = 2

power_supply_status_not_charging :: Word.Word8
power_supply_status_not_charging = 3

power_supply_status_full :: Word.Word8
power_supply_status_full = 4

power_supply_health_unknown :: Word.Word8
power_supply_health_unknown = 0

power_supply_health_good :: Word.Word8
power_supply_health_good = 1

power_supply_health_overheat :: Word.Word8
power_supply_health_overheat = 2

power_supply_health_dead :: Word.Word8
power_supply_health_dead = 3

power_supply_health_overvoltage :: Word.Word8
power_supply_health_overvoltage = 4

power_supply_health_unspec_failure :: Word.Word8
power_supply_health_unspec_failure = 5

power_supply_health_cold :: Word.Word8
power_supply_health_cold = 6

power_supply_health_watchdog_timer_expire :: Word.Word8
power_supply_health_watchdog_timer_expire = 7

power_supply_health_safety_timer_expire :: Word.Word8
power_supply_health_safety_timer_expire = 8

power_supply_technology_unknown :: Word.Word8
power_supply_technology_unknown = 0

power_supply_technology_nimh :: Word.Word8
power_supply_technology_nimh = 1

power_supply_technology_lion :: Word.Word8
power_supply_technology_lion = 2

power_supply_technology_lipo :: Word.Word8
power_supply_technology_lipo = 3

power_supply_technology_life :: Word.Word8
power_supply_technology_life = 4

power_supply_technology_nicd :: Word.Word8
power_supply_technology_nicd = 5

power_supply_technology_limn :: Word.Word8
power_supply_technology_limn = 6

