{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Int32 where
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

data Int32 = Int32 { __data :: P.Int
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Int32)

instance RosBinary Int32 where
  put obj' = put (__data obj')
  get = Int32 <$> get

instance Storable Int32 where
  sizeOf _ = sizeOf (P.undefined::P.Int)
  alignment _ = 8
  peek = SM.runStorable (Int32 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Int32 where
  sourceMD5 _ = "da5909fbe378aeaf85e547e830cc1bb7"
  msgTypeName _ = "std_msgs/Int32"

instance D.Default Int32

