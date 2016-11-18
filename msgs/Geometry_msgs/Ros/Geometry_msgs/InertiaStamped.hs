{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.InertiaStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Inertia as Inertia
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data InertiaStamped = InertiaStamped { _header :: Header.Header
                                     , _inertia :: Inertia.Inertia
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''InertiaStamped)

instance RosBinary InertiaStamped where
  put obj' = put (_header obj') *> put (_inertia obj')
  get = InertiaStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader InertiaStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo InertiaStamped where
  sourceMD5 _ = "ddee48caeab5a966c5e8d166654a9ac7"
  msgTypeName _ = "geometry_msgs/InertiaStamped"

instance D.Default InertiaStamped

