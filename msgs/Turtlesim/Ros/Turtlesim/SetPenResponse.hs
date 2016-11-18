{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.SetPenResponse where
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

data SetPenResponse = SetPenResponse deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary SetPenResponse where
  put _  = pure ()
  get = pure SetPenResponse

instance Storable SetPenResponse where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure SetPenResponse
  poke _ _ = pure ()

instance MsgInfo SetPenResponse where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "turtlesim/SetPenResponse"

instance D.Default SetPenResponse

instance SrvInfo SetPenResponse where
  srvMD5 _ = "9f452acce566bf0c0954594f69a8e41b"
  srvTypeName _ = "turtlesim/SetPen"

