{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_srvs.TriggerResponse where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Data.Word as Word
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data TriggerResponse = TriggerResponse { _success :: P.Bool
                                       , _message :: P.String
                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''TriggerResponse)

instance RosBinary TriggerResponse where
  put obj' = put (_success obj') *> put (_message obj')
  get = TriggerResponse <$> get <*> get

instance MsgInfo TriggerResponse where
  sourceMD5 _ = "937c9679a518e3a18d831e57125ea522"
  msgTypeName _ = "std_srvs/TriggerResponse"

instance D.Default TriggerResponse

instance SrvInfo TriggerResponse where
  srvMD5 _ = "937c9679a518e3a18d831e57125ea522"
  srvTypeName _ = "std_srvs/Trigger"

