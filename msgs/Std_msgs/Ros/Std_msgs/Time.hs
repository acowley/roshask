{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Time where
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

data Time = Time { __data :: ROSTime
                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Time)

instance RosBinary Time where
  put obj' = put (__data obj')
  get = Time <$> get

instance Storable Time where
  sizeOf _ = sizeOf (P.undefined::ROSTime)
  alignment _ = 8
  peek = SM.runStorable (Time <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Time where
  sourceMD5 _ = "cd7166c74c552c311fbcc2fe5a7bc289"
  msgTypeName _ = "std_msgs/Time"

instance D.Default Time

