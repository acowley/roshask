{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.JoyFeedback where
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

data JoyFeedback = JoyFeedback { __type :: Word.Word8
                               , _id :: Word.Word8
                               , _intensity :: P.Float
                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''JoyFeedback)

instance RosBinary JoyFeedback where
  put obj' = put (__type obj') *> put (_id obj') *> put (_intensity obj')
  get = JoyFeedback <$> get <*> get <*> get

instance Storable JoyFeedback where
  sizeOf _ = sizeOf (P.undefined::Word.Word8) +
             sizeOf (P.undefined::Word.Word8) +
             sizeOf (P.undefined::P.Float)
  alignment _ = 8
  peek = SM.runStorable (JoyFeedback <$> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__type obj') *> SM.poke (_id obj') *> SM.poke (_intensity obj')

instance MsgInfo JoyFeedback where
  sourceMD5 _ = "f4dcd73460360d98f36e55ee7f2e46f1"
  msgTypeName _ = "sensor_msgs/JoyFeedback"

instance D.Default JoyFeedback

type_led :: Word.Word8
type_led = 0

type_rumble :: Word.Word8
type_rumble = 1

type_buzzer :: Word.Word8
type_buzzer = 2

