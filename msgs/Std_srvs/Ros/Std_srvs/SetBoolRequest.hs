{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_srvs.SetBoolRequest where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Data.Word as Word
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data SetBoolRequest = SetBoolRequest { __data :: P.Bool
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''SetBoolRequest)

instance RosBinary SetBoolRequest where
  put obj' = put (__data obj')
  get = SetBoolRequest <$> get

instance Storable SetBoolRequest where
  sizeOf _ = sizeOf (P.undefined::P.Bool)
  alignment _ = 8
  peek = SM.runStorable (SetBoolRequest <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo SetBoolRequest where
  sourceMD5 _ = "8b94c1b53db61fb6aed406028ad6332a"
  msgTypeName _ = "std_srvs/SetBoolRequest"

instance D.Default SetBoolRequest

instance SrvInfo SetBoolRequest where
  srvMD5 _ = "09fb03525b03e7ea1fd3992bafd87e16"
  srvTypeName _ = "std_srvs/SetBool"

