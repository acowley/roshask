{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Vector3Stamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Vector3 as Vector3
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Vector3Stamped = Vector3Stamped { _header :: Header.Header
                                     , _vector :: Vector3.Vector3
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Vector3Stamped)

instance RosBinary Vector3Stamped where
  put obj' = put (_header obj') *> put (_vector obj')
  get = Vector3Stamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader Vector3Stamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo Vector3Stamped where
  sourceMD5 _ = "7b324c7325e683bf02a9b14b01090ec7"
  msgTypeName _ = "geometry_msgs/Vector3Stamped"

instance D.Default Vector3Stamped

