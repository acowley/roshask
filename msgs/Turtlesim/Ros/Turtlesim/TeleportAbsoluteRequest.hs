{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.TeleportAbsoluteRequest where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data TeleportAbsoluteRequest = TeleportAbsoluteRequest { _x :: P.Float
                                                       , _y :: P.Float
                                                       , _theta :: P.Float
                                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''TeleportAbsoluteRequest)

instance RosBinary TeleportAbsoluteRequest where
  put obj' = put (_x obj') *> put (_y obj') *> put (_theta obj')
  get = TeleportAbsoluteRequest <$> get <*> get <*> get

instance Storable TeleportAbsoluteRequest where
  sizeOf _ = sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float)
  alignment _ = 8
  peek = SM.runStorable (TeleportAbsoluteRequest <$> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_x obj') *> SM.poke (_y obj') *> SM.poke (_theta obj')

instance MsgInfo TeleportAbsoluteRequest where
  sourceMD5 _ = "a130bc60ee6513855dc62ea83fcc5b20"
  msgTypeName _ = "turtlesim/TeleportAbsoluteRequest"

instance D.Default TeleportAbsoluteRequest

instance SrvInfo TeleportAbsoluteRequest where
  srvMD5 _ = "a130bc60ee6513855dc62ea83fcc5b20"
  srvTypeName _ = "turtlesim/TeleportAbsolute"

