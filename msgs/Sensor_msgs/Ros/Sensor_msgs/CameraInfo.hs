{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.CameraInfo where
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
import qualified Ros.Sensor_msgs.RegionOfInterest as RegionOfInterest
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data CameraInfo = CameraInfo { _header :: Header.Header
                             , _height :: Word.Word32
                             , _width :: Word.Word32
                             , _distortion_model :: P.String
                             , _d :: V.Vector P.Double
                             , _k :: V.Vector P.Double
                             , _r :: V.Vector P.Double
                             , _p :: V.Vector P.Double
                             , _binning_x :: Word.Word32
                             , _binning_y :: Word.Word32
                             , _roi :: RegionOfInterest.RegionOfInterest
                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''CameraInfo)

instance RosBinary CameraInfo where
  put obj' = put (_header obj') *> put (_height obj') *> put (_width obj') *> put (_distortion_model obj') *> put (_d obj') *> put (_k obj') *> put (_r obj') *> put (_p obj') *> put (_binning_x obj') *> put (_binning_y obj') *> put (_roi obj')
  get = CameraInfo <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader CameraInfo where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo CameraInfo where
  sourceMD5 _ = "c9a58c1b0b154e0e6da7578cb991d214"
  msgTypeName _ = "sensor_msgs/CameraInfo"

instance D.Default CameraInfo

