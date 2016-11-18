{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.UInt8 where
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

data UInt8 = UInt8 { __data :: Word.Word8
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''UInt8)

instance RosBinary UInt8 where
  put obj' = put (__data obj')
  get = UInt8 <$> get

instance Storable UInt8 where
  sizeOf _ = sizeOf (P.undefined::Word.Word8)
  alignment _ = 8
  peek = SM.runStorable (UInt8 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo UInt8 where
  sourceMD5 _ = "7c8164229e7d2c17eb95e9231617fdee"
  msgTypeName _ = "std_msgs/UInt8"

instance D.Default UInt8

