{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Actionlib_msgs.GoalStatusArray where
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
import qualified Ros.Actionlib_msgs.GoalStatus as GoalStatus
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GoalStatusArray = GoalStatusArray { _header :: Header.Header
                                       , _status_list :: [GoalStatus.GoalStatus]
                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GoalStatusArray)

instance RosBinary GoalStatusArray where
  put obj' = put (_header obj') *> putList (_status_list obj')
  get = GoalStatusArray <$> get <*> getList
  putMsg = putStampedMsg

instance HasHeader GoalStatusArray where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo GoalStatusArray where
  sourceMD5 _ = "8b2b82f13216d0a8ea88bd3af735e619"
  msgTypeName _ = "actionlib_msgs/GoalStatusArray"

instance D.Default GoalStatusArray

