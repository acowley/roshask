{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.JoyFeedbackArray where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Vector.Storable as V
import qualified Ros.Sensor_msgs.JoyFeedback as JoyFeedback
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data JoyFeedbackArray = JoyFeedbackArray { _array :: V.Vector JoyFeedback.JoyFeedback
                                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''JoyFeedbackArray)

instance RosBinary JoyFeedbackArray where
  put obj' = put (_array obj')
  get = JoyFeedbackArray <$> get

instance MsgInfo JoyFeedbackArray where
  sourceMD5 _ = "cde5730a895b1fc4dee6f91b754b213d"
  msgTypeName _ = "sensor_msgs/JoyFeedbackArray"

instance D.Default JoyFeedbackArray

