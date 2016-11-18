{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetPlanResponse where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Ros.Nav_msgs.Path as Path
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GetPlanResponse = GetPlanResponse { _plan :: Path.Path
                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GetPlanResponse)

instance RosBinary GetPlanResponse where
  put obj' = put (_plan obj')
  get = GetPlanResponse <$> get

instance MsgInfo GetPlanResponse where
  sourceMD5 _ = "0002bc113c0259d71f6cf8cbc9430e18"
  msgTypeName _ = "nav_msgs/GetPlanResponse"

instance D.Default GetPlanResponse

instance SrvInfo GetPlanResponse where
  srvMD5 _ = "421c8ea4d21c6c9db7054b4bbdf1e024"
  srvTypeName _ = "nav_msgs/GetPlan"

