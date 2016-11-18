{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Float64 where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Float64 = Float64 { __data :: P.Double
                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Float64)

instance RosBinary Float64 where
  put obj' = put (__data obj')
  get = Float64 <$> get

instance Storable Float64 where
  sizeOf _ = sizeOf (P.undefined::P.Double)
  alignment _ = 8
  peek = SM.runStorable (Float64 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Float64 where
  sourceMD5 _ = "fdb28210bfa9d7c91146260178d9a584"
  msgTypeName _ = "std_msgs/Float64"

instance D.Default Float64

