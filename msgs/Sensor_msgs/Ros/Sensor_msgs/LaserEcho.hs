{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.LaserEcho where
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

data LaserEcho = LaserEcho { _echoes :: V.Vector P.Float
                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''LaserEcho)

instance RosBinary LaserEcho where
  put obj' = put (_echoes obj')
  get = LaserEcho <$> get

instance MsgInfo LaserEcho where
  sourceMD5 _ = "8bc5ae449b200fba4d552b4225586696"
  msgTypeName _ = "sensor_msgs/LaserEcho"

instance D.Default LaserEcho

