{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
module Ros.Test_srvs.EmptyRequest where
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

data EmptyRequest = EmptyRequest deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary EmptyRequest where
  put _  = pure ()
  get = pure EmptyRequest

instance Storable EmptyRequest where
  sizeOf _ = 1
  alignment _ = 1
  peek _ = pure EmptyRequest
  poke _ _ = pure ()

instance MsgInfo EmptyRequest where
  sourceMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  msgTypeName _ = "test_srvs/EmptyRequest"

instance D.Default EmptyRequest

instance SrvInfo EmptyRequest where
  srvMD5 _ = "d41d8cd98f00b204e9800998ecf8427e"
  srvTypeName _ = "test_srvs/Empty"

