{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.UInt16 where
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

data UInt16 = UInt16 { __data :: Word.Word16
                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''UInt16)

instance RosBinary UInt16 where
  put obj' = put (__data obj')
  get = UInt16 <$> get

instance Storable UInt16 where
  sizeOf _ = sizeOf (P.undefined::Word.Word16)
  alignment _ = 8
  peek = SM.runStorable (UInt16 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo UInt16 where
  sourceMD5 _ = "1df79edf208b629fe6b81923a544552d"
  msgTypeName _ = "std_msgs/UInt16"

instance D.Default UInt16

