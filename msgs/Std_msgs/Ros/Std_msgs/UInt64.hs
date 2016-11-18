{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.UInt64 where
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

data UInt64 = UInt64 { __data :: Word.Word64
                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''UInt64)

instance RosBinary UInt64 where
  put obj' = put (__data obj')
  get = UInt64 <$> get

instance Storable UInt64 where
  sizeOf _ = sizeOf (P.undefined::Word.Word64)
  alignment _ = 8
  peek = SM.runStorable (UInt64 <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo UInt64 where
  sourceMD5 _ = "1b2a79973e8bf53d7b53acb71299cb57"
  msgTypeName _ = "std_msgs/UInt64"

instance D.Default UInt64

