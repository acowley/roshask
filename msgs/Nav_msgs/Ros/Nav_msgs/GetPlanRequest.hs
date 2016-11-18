{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetPlanRequest where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Ros.Geometry_msgs.PoseStamped as PoseStamped
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data GetPlanRequest = GetPlanRequest { _start :: PoseStamped.PoseStamped
                                     , _goal :: PoseStamped.PoseStamped
                                     , _tolerance :: P.Float
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''GetPlanRequest)

instance RosBinary GetPlanRequest where
  put obj' = put (_start obj') *> put (_goal obj') *> put (_tolerance obj')
  get = GetPlanRequest <$> get <*> get <*> get

instance MsgInfo GetPlanRequest where
  sourceMD5 _ = "e25a43e0752bcca599a8c2eef8282df8"
  msgTypeName _ = "nav_msgs/GetPlanRequest"

instance D.Default GetPlanRequest

instance SrvInfo GetPlanRequest where
  srvMD5 _ = "421c8ea4d21c6c9db7054b4bbdf1e024"
  srvTypeName _ = "nav_msgs/GetPlan"

