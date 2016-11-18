{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.PoseStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Pose as Pose
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data PoseStamped = PoseStamped { _header :: Header.Header
                               , _pose :: Pose.Pose
                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''PoseStamped)

instance RosBinary PoseStamped where
  put obj' = put (_header obj') *> put (_pose obj')
  get = PoseStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader PoseStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo PoseStamped where
  sourceMD5 _ = "d3812c3cbc69362b77dc0b19b345f8f5"
  msgTypeName _ = "geometry_msgs/PoseStamped"

instance D.Default PoseStamped

