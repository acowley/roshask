{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Duration where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.RosTypes
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Duration = Duration { __data :: ROSDuration
                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Duration)

instance RosBinary Duration where
  put obj' = put (__data obj')
  get = Duration <$> get

instance Storable Duration where
  sizeOf _ = sizeOf (P.undefined::ROSDuration)
  alignment _ = 8
  peek = SM.runStorable (Duration <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Duration where
  sourceMD5 _ = "3e286caf4241d664e55f3ad380e2ae46"
  msgTypeName _ = "std_msgs/Duration"

instance D.Default Duration

