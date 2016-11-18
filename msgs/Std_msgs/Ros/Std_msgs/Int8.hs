{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Int8 where
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

data Int8 = Int8 { __data :: Int.Int8
                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Int8)

instance RosBinary Int8 where
  put obj' = put (__data obj')
  get = Int8 <$> get

instance Storable Int8 where
  sizeOf _ = sizeOf (P.undefined::Int.Int8)
  alignment _ = 8
  peek = SM.runStorable (Int8 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Int8 where
  sourceMD5 _ = "27ffa0c9c4b8fb8492252bcad9e5c57b"
  msgTypeName _ = "std_msgs/Int8"

instance D.Default Int8

