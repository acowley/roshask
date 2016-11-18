{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.QuaternionStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Quaternion as Quaternion
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data QuaternionStamped = QuaternionStamped { _header :: Header.Header
                                           , _quaternion :: Quaternion.Quaternion
                                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''QuaternionStamped)

instance RosBinary QuaternionStamped where
  put obj' = put (_header obj') *> put (_quaternion obj')
  get = QuaternionStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader QuaternionStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo QuaternionStamped where
  sourceMD5 _ = "e57f1e547e0e1fd13504588ffc8334e2"
  msgTypeName _ = "geometry_msgs/QuaternionStamped"

instance D.Default QuaternionStamped

