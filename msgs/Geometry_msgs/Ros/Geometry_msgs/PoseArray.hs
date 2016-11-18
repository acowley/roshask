{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.PoseArray where
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
import qualified Ros.Geometry_msgs.Pose as Pose
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data PoseArray = PoseArray { _header :: Header.Header
                           , _poses :: V.Vector Pose.Pose
                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''PoseArray)

instance RosBinary PoseArray where
  put obj' = put (_header obj') *> put (_poses obj')
  get = PoseArray <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader PoseArray where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo PoseArray where
  sourceMD5 _ = "916c28c5764443f268b296bb671b9d97"
  msgTypeName _ = "geometry_msgs/PoseArray"

instance D.Default PoseArray

