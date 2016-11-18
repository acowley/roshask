{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.Range where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Data.Word as Word
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Range = Range { _header :: Header.Header
                   , _radiation_type :: Word.Word8
                   , _field_of_view :: P.Float
                   , _min_range :: P.Float
                   , _max_range :: P.Float
                   , _range :: P.Float
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Range)

instance RosBinary Range where
  put obj' = put (_header obj') *> put (_radiation_type obj') *> put (_field_of_view obj') *> put (_min_range obj') *> put (_max_range obj') *> put (_range obj')
  get = Range <$> get <*> get <*> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader Range where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo Range where
  sourceMD5 _ = "c005c34273dc426c67a020a87bc24148"
  msgTypeName _ = "sensor_msgs/Range"

instance D.Default Range

ultrasound :: Word.Word8
ultrasound = 0

infrared :: Word.Word8
infrared = 1

