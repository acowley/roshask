{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.NavSatFix where
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
import qualified Ros.Sensor_msgs.NavSatStatus as NavSatStatus
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data NavSatFix = NavSatFix { _header :: Header.Header
                           , _status :: NavSatStatus.NavSatStatus
                           , _latitude :: P.Double
                           , _longitude :: P.Double
                           , _altitude :: P.Double
                           , _position_covariance :: V.Vector P.Double
                           , _position_covariance_type :: Word.Word8
                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''NavSatFix)

instance RosBinary NavSatFix where
  put obj' = put (_header obj') *> put (_status obj') *> put (_latitude obj') *> put (_longitude obj') *> put (_altitude obj') *> put (_position_covariance obj') *> put (_position_covariance_type obj')
  get = NavSatFix <$> get <*> get <*> get <*> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader NavSatFix where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo NavSatFix where
  sourceMD5 _ = "2d3a8cd499b9b4a0249fb98fd05cfa48"
  msgTypeName _ = "sensor_msgs/NavSatFix"

instance D.Default NavSatFix

covariance_type_unknown :: Word.Word8
covariance_type_unknown = 0

covariance_type_approximated :: Word.Word8
covariance_type_approximated = 1

covariance_type_diagonal_known :: Word.Word8
covariance_type_diagonal_known = 2

covariance_type_known :: Word.Word8
covariance_type_known = 3

