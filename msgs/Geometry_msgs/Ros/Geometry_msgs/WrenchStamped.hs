{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.WrenchStamped where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Geometry_msgs.Wrench as Wrench
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data WrenchStamped = WrenchStamped { _header :: Header.Header
                                   , _wrench :: Wrench.Wrench
                                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''WrenchStamped)

instance RosBinary WrenchStamped where
  put obj' = put (_header obj') *> put (_wrench obj')
  get = WrenchStamped <$> get <*> get
  putMsg = putStampedMsg

instance HasHeader WrenchStamped where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo WrenchStamped where
  sourceMD5 _ = "d78d3cb249ce23087ade7e7d0c40cfa7"
  msgTypeName _ = "geometry_msgs/WrenchStamped"

instance D.Default WrenchStamped

