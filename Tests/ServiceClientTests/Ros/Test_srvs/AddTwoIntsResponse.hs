{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
module Ros.Test_srvs.AddTwoIntsResponse where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Data.Int as Int
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM

data AddTwoIntsResponse = AddTwoIntsResponse { sum :: Int.Int64
                                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary AddTwoIntsResponse where
  put obj' = put (sum obj')
  get = AddTwoIntsResponse <$> get

instance Storable AddTwoIntsResponse where
  sizeOf _ = sizeOf (P.undefined::Int.Int64)
  alignment _ = 8
  peek = SM.runStorable (AddTwoIntsResponse <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (sum obj')

instance MsgInfo AddTwoIntsResponse where
  sourceMD5 _ = "b88405221c77b1878a3cbbfff53428d7"
  msgTypeName _ = "test_srvs/AddTwoIntsResponse"

instance D.Default AddTwoIntsResponse

instance SrvInfo AddTwoIntsResponse where
  srvMD5 _ = "6a2e34150c00229791cc89ff309fff21"
  srvTypeName _ = "test_srvs/AddTwoInts"

