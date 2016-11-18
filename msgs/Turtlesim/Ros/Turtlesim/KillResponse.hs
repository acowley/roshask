{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.KillResponse where
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

data KillResponse = KillResponse deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary KillResponse where
  put _  = pure ()
  get = pure KillResponse

instance Storable KillResponse where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure KillResponse
  poke _ _ = pure ()

instance MsgInfo KillResponse where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "turtlesim/KillResponse"

instance D.Default KillResponse

instance SrvInfo KillResponse where
  srvMD5 _ = "c1f3d28f1b044c871e6eff2e9fc3c667"
  srvTypeName _ = "turtlesim/Kill"

