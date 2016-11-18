{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.ColorRGBA where
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

data ColorRGBA = ColorRGBA { _r :: P.Float
                           , _g :: P.Float
                           , _b :: P.Float
                           , _a :: P.Float
                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''ColorRGBA)

instance RosBinary ColorRGBA where
  put obj' = put (_r obj') *> put (_g obj') *> put (_b obj') *> put (_a obj')
  get = ColorRGBA <$> get <*> get <*> get <*> get

instance Storable ColorRGBA where
  sizeOf _ = sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float)
  alignment _ = 8
  peek = SM.runStorable (ColorRGBA <$> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_r obj') *> SM.poke (_g obj') *> SM.poke (_b obj') *> SM.poke (_a obj')

instance MsgInfo ColorRGBA where
  sourceMD5 _ = "a29a96539573343b1310c73607334b00"
  msgTypeName _ = "std_msgs/ColorRGBA"

instance D.Default ColorRGBA

