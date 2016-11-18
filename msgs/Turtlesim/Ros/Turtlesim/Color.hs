{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.Color where
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

data Color = Color { _r :: Word.Word8
                   , _g :: Word.Word8
                   , _b :: Word.Word8
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Color)

instance RosBinary Color where
  put obj' = put (_r obj') *> put (_g obj') *> put (_b obj')
  get = Color <$> get <*> get <*> get

instance Storable Color where
  sizeOf _ = sizeOf (P.undefined::Word.Word8) +
             sizeOf (P.undefined::Word.Word8) +
             sizeOf (P.undefined::Word.Word8)
  alignment _ = 8
  peek = SM.runStorable (Color <$> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_r obj') *> SM.poke (_g obj') *> SM.poke (_b obj')

instance MsgInfo Color where
  sourceMD5 _ = "353891e354491c51aabe32df673fb446"
  msgTypeName _ = "turtlesim/Color"

instance D.Default Color

