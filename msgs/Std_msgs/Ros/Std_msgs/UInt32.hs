{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.UInt32 where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Word as Word
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data UInt32 = UInt32 { __data :: Word.Word32
                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''UInt32)

instance RosBinary UInt32 where
  put obj' = put (__data obj')
  get = UInt32 <$> get

instance Storable UInt32 where
  sizeOf _ = sizeOf (P.undefined::Word.Word32)
  alignment _ = 8
  peek = SM.runStorable (UInt32 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo UInt32 where
  sourceMD5 _ = "304a39449588c7f8ce2df6e8001c5fce"
  msgTypeName _ = "std_msgs/UInt32"

instance D.Default UInt32

