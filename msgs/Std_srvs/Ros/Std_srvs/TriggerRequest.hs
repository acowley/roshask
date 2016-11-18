{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_srvs.TriggerRequest where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import Foreign.Storable (Storable(..))
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data TriggerRequest = TriggerRequest deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary TriggerRequest where
  put _  = pure ()
  get = pure TriggerRequest

instance Storable TriggerRequest where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure TriggerRequest
  poke _ _ = pure ()

instance MsgInfo TriggerRequest where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "std_srvs/TriggerRequest"

instance D.Default TriggerRequest

instance SrvInfo TriggerRequest where
  srvMD5 _ = "937c9679a518e3a18d831e57125ea522"
  srvTypeName _ = "std_srvs/Trigger"

