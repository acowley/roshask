{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.TeleportAbsoluteResponse where
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

data TeleportAbsoluteResponse = TeleportAbsoluteResponse deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary TeleportAbsoluteResponse where
  put _  = pure ()
  get = pure TeleportAbsoluteResponse

instance Storable TeleportAbsoluteResponse where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure TeleportAbsoluteResponse
  poke _ _ = pure ()

instance MsgInfo TeleportAbsoluteResponse where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "turtlesim/TeleportAbsoluteResponse"

instance D.Default TeleportAbsoluteResponse

instance SrvInfo TeleportAbsoluteResponse where
  srvMD5 _ = "a130bc60ee6513855dc62ea83fcc5b20"
  srvTypeName _ = "turtlesim/TeleportAbsolute"

