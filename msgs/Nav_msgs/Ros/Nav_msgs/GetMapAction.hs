{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetMapAction where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Ros.Nav_msgs.GetMapActionFeedback as GetMapActionFeedback
import qualified Ros.Nav_msgs.GetMapActionGoal as GetMapActionGoal
import qualified Ros.Nav_msgs.GetMapActionResult as GetMapActionResult
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GetMapAction = GetMapAction { _action_goal :: GetMapActionGoal.GetMapActionGoal
                                 , _action_result :: GetMapActionResult.GetMapActionResult
                                 , _action_feedback :: GetMapActionFeedback.GetMapActionFeedback
                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GetMapAction)

instance RosBinary GetMapAction where
  put obj' = put (_action_goal obj') *> put (_action_result obj') *> put (_action_feedback obj')
  get = GetMapAction <$> get <*> get <*> get

instance MsgInfo GetMapAction where
  sourceMD5 _ = "e611ad23fbf237c031b7536416dc7cd7"
  msgTypeName _ = "nav_msgs/GetMapAction"

instance D.Default GetMapAction

