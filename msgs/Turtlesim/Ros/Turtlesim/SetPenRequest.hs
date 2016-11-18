{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.SetPenRequest where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import qualified Data.Word as Word
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data SetPenRequest = SetPenRequest { _r :: Word.Word8
                                   , _g :: Word.Word8
                                   , _b :: Word.Word8
                                   , _width :: Word.Word8
                                   , _off :: Word.Word8
                                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''SetPenRequest)

instance RosBinary SetPenRequest where
  put obj' = put (_r obj') *> put (_g obj') *> put (_b obj') *> put (_width obj') *> put (_off obj')
  get = SetPenRequest <$> get <*> get <*> get <*> get <*> get

instance Storable SetPenRequest where
  sizeOf _ = sizeOf (P.undefined::Word.Word8) +
             sizeOf (P.undefined::Word.Word8) +
             sizeOf (P.undefined::Word.Word8) +
             sizeOf (P.undefined::Word.Word8) +
             sizeOf (P.undefined::Word.Word8)
  alignment _ = 8
  peek = SM.runStorable (SetPenRequest <$> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_r obj') *> SM.poke (_g obj') *> SM.poke (_b obj') *> SM.poke (_width obj') *> SM.poke (_off obj')

instance MsgInfo SetPenRequest where
  sourceMD5 _ = "9f452acce566bf0c0954594f69a8e41b"
  msgTypeName _ = "turtlesim/SetPenRequest"

instance D.Default SetPenRequest

instance SrvInfo SetPenRequest where
  srvMD5 _ = "9f452acce566bf0c0954594f69a8e41b"
  srvTypeName _ = "turtlesim/SetPen"

