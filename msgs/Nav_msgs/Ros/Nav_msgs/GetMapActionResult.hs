{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetMapActionResult where
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
import qualified Ros.Nav_msgs.GetMapResult as GetMapResult
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GetMapActionResult = GetMapActionResult { _header :: Header.Header
                                             , _status :: GoalStatus.GoalStatus
                                             , _result :: GetMapResult.GetMapResult
                                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GetMapActionResult)

instance RosBinary GetMapActionResult where
  put obj' = put (_header obj') *> put (_status obj') *> put (_result obj')
  get = GetMapActionResult <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader GetMapActionResult where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo GetMapActionResult where
  sourceMD5 _ = "ac66e5b9a79bb4bbd33dab245236c892"
  msgTypeName _ = "nav_msgs/GetMapActionResult"

instance D.Default GetMapActionResult

