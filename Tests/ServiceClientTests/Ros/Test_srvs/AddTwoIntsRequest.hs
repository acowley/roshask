{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}
module Ros.Test_srvs.AddTwoIntsRequest where
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

data AddTwoIntsRequest = AddTwoIntsRequest { a :: Int.Int64
                                           , b :: Int.Int64
                                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

instance RosBinary AddTwoIntsRequest where
  put obj' = put (a obj') *> put (b obj')
  get = AddTwoIntsRequest <$> get <*> get

instance Storable AddTwoIntsRequest where
  sizeOf _ = sizeOf (P.undefined::Int.Int64) +
             sizeOf (P.undefined::Int.Int64)
  alignment _ = 8
  peek = SM.runStorable (AddTwoIntsRequest <$> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (a obj') *> SM.poke (b obj')

instance MsgInfo AddTwoIntsRequest where
  sourceMD5 _ = "36d09b846be0b371c5f190354dd3153e"
  msgTypeName _ = "test_srvs/AddTwoIntsRequest"

instance D.Default AddTwoIntsRequest

instance SrvInfo AddTwoIntsRequest where
  srvMD5 _ = "6a2e34150c00229791cc89ff309fff21"
  srvTypeName _ = "test_srvs/AddTwoInts"

