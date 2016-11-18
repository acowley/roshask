{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Byte where
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

data Byte = Byte { __data :: Word.Word8
                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Byte)

instance RosBinary Byte where
  put obj' = put (__data obj')
  get = Byte <$> get

instance Storable Byte where
  sizeOf _ = sizeOf (P.undefined::Word.Word8)
  alignment _ = 8
  peek = SM.runStorable (Byte <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Byte where
  sourceMD5 _ = "ad736a2e8818154c487bb80fe42ce43b"
  msgTypeName _ = "std_msgs/Byte"

instance D.Default Byte

