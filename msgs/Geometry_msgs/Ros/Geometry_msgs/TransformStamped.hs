{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.TransformStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Transform as Transform
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data TransformStamped = TransformStamped { _header :: Header.Header
                                         , _child_frame_id :: P.String
                                         , _transform :: Transform.Transform
                                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''TransformStamped)

instance RosBinary TransformStamped where
  put obj' = put (_header obj') *> put (_child_frame_id obj') *> put (_transform obj')
  get = TransformStamped <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader TransformStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo TransformStamped where
  sourceMD5 _ = "b5764a33bfeb3588febc2682852579b0"
  msgTypeName _ = "geometry_msgs/TransformStamped"

instance D.Default TransformStamped

