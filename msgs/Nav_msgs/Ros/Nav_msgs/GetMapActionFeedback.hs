{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetMapActionFeedback where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Actionlib_msgs.GoalStatus as GoalStatus
import qualified Ros.Nav_msgs.GetMapFeedback as GetMapFeedback
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GetMapActionFeedback = GetMapActionFeedback { _header :: Header.Header
                                                 , _status :: GoalStatus.GoalStatus
                                                 , _feedback :: GetMapFeedback.GetMapFeedback
                                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GetMapActionFeedback)

instance RosBinary GetMapActionFeedback where
  put obj' = put (_header obj') *> put (_status obj') *> put (_feedback obj')
  get = GetMapActionFeedback <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader GetMapActionFeedback where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo GetMapActionFeedback where
  sourceMD5 _ = "aae20e09065c3809e8a8e87c4c8953fd"
  msgTypeName _ = "nav_msgs/GetMapActionFeedback"

instance D.Default GetMapActionFeedback

