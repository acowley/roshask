{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.MagneticField where
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
import qualified Ros.Geometry_msgs.Vector3 as Vector3
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data MagneticField = MagneticField { _header :: Header.Header
                                   , _magnetic_field :: Vector3.Vector3
                                   , _magnetic_field_covariance :: V.Vector P.Double
                                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''MagneticField)

instance RosBinary MagneticField where
  put obj' = put (_header obj') *> put (_magnetic_field obj') *> put (_magnetic_field_covariance obj')
  get = MagneticField <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader MagneticField where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo MagneticField where
  sourceMD5 _ = "2f3b0b43eed0c9501de0fa3ff89a45aa"
  msgTypeName _ = "sensor_msgs/MagneticField"

instance D.Default MagneticField

