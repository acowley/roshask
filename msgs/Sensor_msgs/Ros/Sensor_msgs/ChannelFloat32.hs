{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.ChannelFloat32 where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Vector.Storable as V
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data ChannelFloat32 = ChannelFloat32 { _name :: P.String
                                     , _values :: V.Vector P.Float
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''ChannelFloat32)

instance RosBinary ChannelFloat32 where
  put obj' = put (_name obj') *> put (_values obj')
  get = ChannelFloat32 <$> get <*> get

instance MsgInfo ChannelFloat32 where
  sourceMD5 _ = "3d40139cdd33dfedcb71ffeeeb42ae7f"
  msgTypeName _ = "sensor_msgs/ChannelFloat32"

instance D.Default ChannelFloat32

