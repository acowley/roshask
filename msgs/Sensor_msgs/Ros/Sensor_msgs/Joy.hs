{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.Joy where
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
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Joy = Joy { _header :: Header.Header
               , _axes :: V.Vector P.Float
               , _buttons :: V.Vector P.Int
               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Joy)

instance RosBinary Joy where
  put obj' = put (_header obj') *> put (_axes obj') *> put (_buttons obj')
  get = Joy <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader Joy where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo Joy where
  sourceMD5 _ = "5a9ea5f83505693b71e785041e67a8bb"
  msgTypeName _ = "sensor_msgs/Joy"

instance D.Default Joy

