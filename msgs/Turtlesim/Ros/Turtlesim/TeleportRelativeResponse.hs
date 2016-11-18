{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.TeleportRelativeResponse where
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

data TeleportRelativeResponse = TeleportRelativeResponse deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary TeleportRelativeResponse where
  put _  = pure ()
  get = pure TeleportRelativeResponse

instance Storable TeleportRelativeResponse where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure TeleportRelativeResponse
  poke _ _ = pure ()

instance MsgInfo TeleportRelativeResponse where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "turtlesim/TeleportRelativeResponse"

instance D.Default TeleportRelativeResponse

instance SrvInfo TeleportRelativeResponse where
  srvMD5 _ = "9d5c2dcd348ac8f76ce2a4307bd63a13"
  srvTypeName _ = "turtlesim/TeleportRelative"

