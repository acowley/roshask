{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.Path where
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
import qualified Ros.Geometry_msgs.PoseStamped as PoseStamped
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Path = Path { _header :: Header.Header
                 , _poses :: [PoseStamped.PoseStamped]
                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Path)

instance RosBinary Path where
  put obj' = put (_header obj') *> putList (_poses obj')
  get = Path <$> get <*> getList
  putMsg = putStampedMsg

instance HasHeader Path where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo Path where
  sourceMD5 _ = "6227e2b7e9cce15051f669a5e197bbf7"
  msgTypeName _ = "nav_msgs/Path"

instance D.Default Path

