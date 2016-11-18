{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Int16 where
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

data Int16 = Int16 { __data :: Int.Int16
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Int16)

instance RosBinary Int16 where
  put obj' = put (__data obj')
  get = Int16 <$> get

instance Storable Int16 where
  sizeOf _ = sizeOf (P.undefined::Int.Int16)
  alignment _ = 8
  peek = SM.runStorable (Int16 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Int16 where
  sourceMD5 _ = "8524586e34fbd7cb1c08c5f5f1ca0e57"
  msgTypeName _ = "std_msgs/Int16"

instance D.Default Int16

