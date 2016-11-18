{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.TwistStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Twist as Twist
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data TwistStamped = TwistStamped { _header :: Header.Header
                                 , _twist :: Twist.Twist
                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''TwistStamped)

instance RosBinary TwistStamped where
  put obj' = put (_header obj') *> put (_twist obj')
  get = TwistStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader TwistStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo TwistStamped where
  sourceMD5 _ = "98d34b0043a2093cf9d9345ab6eef12e"
  msgTypeName _ = "geometry_msgs/TwistStamped"

instance D.Default TwistStamped

