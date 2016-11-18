{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.KillRequest where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.SrvInfo
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data KillRequest = KillRequest { _name :: P.String
                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''KillRequest)

instance RosBinary KillRequest where
  put obj' = put (_name obj')
  get = KillRequest <$> get

instance MsgInfo KillRequest where
  sourceMD5 _ = "c1f3d28f1b044c871e6eff2e9fc3c667"
  msgTypeName _ = "turtlesim/KillRequest"

instance D.Default KillRequest

instance SrvInfo KillRequest where
  srvMD5 _ = "c1f3d28f1b044c871e6eff2e9fc3c667"
  srvTypeName _ = "turtlesim/Kill"

