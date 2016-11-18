{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.PointStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Point as Point
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data PointStamped = PointStamped { _header :: Header.Header
                                 , _point :: Point.Point
                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''PointStamped)

instance RosBinary PointStamped where
  put obj' = put (_header obj') *> put (_point obj')
  get = PointStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader PointStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo PointStamped where
  sourceMD5 _ = "c63aecb41bfdfd6b7e1fac37c7cbe7bf"
  msgTypeName _ = "geometry_msgs/PointStamped"

instance D.Default PointStamped

