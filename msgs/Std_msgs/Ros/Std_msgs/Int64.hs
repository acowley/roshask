{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Int64 where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Int as Int
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Int64 = Int64 { __data :: Int.Int64
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Int64)

instance RosBinary Int64 where
  put obj' = put (__data obj')
  get = Int64 <$> get

instance Storable Int64 where
  sizeOf _ = sizeOf (P.undefined::Int.Int64)
  alignment _ = 8
  peek = SM.runStorable (Int64 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Int64 where
  sourceMD5 _ = "34add168574510e6e17f5d23ecc077ef"
  msgTypeName _ = "std_msgs/Int64"

instance D.Default Int64

