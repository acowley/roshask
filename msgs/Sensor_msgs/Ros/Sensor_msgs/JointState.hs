{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.JointState where
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

data JointState = JointState { _header :: Header.Header
                             , _name :: [P.String]
                             , _position :: V.Vector P.Double
                             , _velocity :: V.Vector P.Double
                             , _effort :: V.Vector P.Double
                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''JointState)

instance RosBinary JointState where
  put obj' = put (_header obj') *> putList (_name obj') *> put (_position obj') *> put (_velocity obj') *> put (_effort obj')
  get = JointState <$> get <*> getList <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader JointState where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo JointState where
  sourceMD5 _ = "3066dcd76a6cfaef579bd0f34173e9fd"
  msgTypeName _ = "sensor_msgs/JointState"

instance D.Default JointState

