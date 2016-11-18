{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.PolygonStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Polygon as Polygon
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data PolygonStamped = PolygonStamped { _header :: Header.Header
                                     , _polygon :: Polygon.Polygon
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''PolygonStamped)

instance RosBinary PolygonStamped where
  put obj' = put (_header obj') *> put (_polygon obj')
  get = PolygonStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader PolygonStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo PolygonStamped where
  sourceMD5 _ = "c6be8f7dc3bee7fe9e8d296070f53340"
  msgTypeName _ = "geometry_msgs/PolygonStamped"

instance D.Default PolygonStamped

