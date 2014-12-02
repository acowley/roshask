{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
module Ros.Test_srvs.EmptyResponse where
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

data EmptyResponse = EmptyResponse deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary EmptyResponse where
  put _  = pure ()
  get = pure EmptyResponse

instance Storable EmptyResponse where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure EmptyResponse
  poke _ _ = pure ()

instance MsgInfo EmptyResponse where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "test_srvs/EmptyResponse"

instance D.Default EmptyResponse

instance SrvInfo EmptyResponse where
  srvMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  srvTypeName _ = "test_srvs/Empty"

