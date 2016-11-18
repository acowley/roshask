{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetMapActionGoal where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Ros.Actionlib_msgs.GoalID as GoalID
import qualified Ros.Nav_msgs.GetMapGoal as GetMapGoal
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GetMapActionGoal = GetMapActionGoal { _header :: Header.Header
                                         , _goal_id :: GoalID.GoalID
                                         , _goal :: GetMapGoal.GetMapGoal
                                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GetMapActionGoal)

instance RosBinary GetMapActionGoal where
  put obj' = put (_header obj') *> put (_goal_id obj') *> put (_goal obj')
  get = GetMapActionGoal <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader GetMapActionGoal where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo GetMapActionGoal where
  sourceMD5 _ = "4b30be6cd12b9e72826df56b481f40e0"
  msgTypeName _ = "nav_msgs/GetMapActionGoal"

instance D.Default GetMapActionGoal

