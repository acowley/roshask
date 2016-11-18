{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Float32 where
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

data Float32 = Float32 { __data :: P.Float
                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Float32)

instance RosBinary Float32 where
  put obj' = put (__data obj')
  get = Float32 <$> get

instance Storable Float32 where
  sizeOf _ = sizeOf (P.undefined::P.Float)
  alignment _ = 8
  peek = SM.runStorable (Float32 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Float32 where
  sourceMD5 _ = "73fcbf46b49191e672908e50842a83d4"
  msgTypeName _ = "std_msgs/Float32"

instance D.Default Float32

