{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.TeleportRelativeRequest where
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

data TeleportRelativeRequest = TeleportRelativeRequest { _linear :: P.Float
                                                       , _angular :: P.Float
                                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''TeleportRelativeRequest)

instance RosBinary TeleportRelativeRequest where
  put obj' = put (_linear obj') *> put (_angular obj')
  get = TeleportRelativeRequest <$> get <*> get

instance Storable TeleportRelativeRequest where
  sizeOf _ = sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float)
  alignment _ = 8
  peek = SM.runStorable (TeleportRelativeRequest <$> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_linear obj') *> SM.poke (_angular obj')

instance MsgInfo TeleportRelativeRequest where
  sourceMD5 _ = "9d5c2dcd348ac8f76ce2a4307bd63a13"
  msgTypeName _ = "turtlesim/TeleportRelativeRequest"

instance D.Default TeleportRelativeRequest

instance SrvInfo TeleportRelativeRequest where
  srvMD5 _ = "9d5c2dcd348ac8f76ce2a4307bd63a13"
  srvTypeName _ = "turtlesim/TeleportRelative"

