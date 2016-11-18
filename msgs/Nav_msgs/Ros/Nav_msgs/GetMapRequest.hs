{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.GetMapRequest where
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

data GetMapRequest = GetMapRequest deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary GetMapRequest where
  put _  = pure ()
  get = pure GetMapRequest

instance Storable GetMapRequest where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure GetMapRequest
  poke _ _ = pure ()

instance MsgInfo GetMapRequest where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "nav_msgs/GetMapRequest"

instance D.Default GetMapRequest

instance SrvInfo GetMapRequest where
  srvMD5 _ = "6cdd0a18e0aff5b0a3ca2326a89b54ff"
  srvTypeName _ = "nav_msgs/GetMap"

