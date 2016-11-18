{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.SpawnResponse where
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

data SpawnResponse = SpawnResponse { _name :: P.String
                                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''SpawnResponse)

instance RosBinary SpawnResponse where
  put obj' = put (_name obj')
  get = SpawnResponse <$> get

instance MsgInfo SpawnResponse where
  sourceMD5 _ = "c1f3d28f1b044c871e6eff2e9fc3c667"
  msgTypeName _ = "turtlesim/SpawnResponse"

instance D.Default SpawnResponse

instance SrvInfo SpawnResponse where
  srvMD5 _ = "0b2d2e872a8e2887d5ed626f2bf2c561"
  srvTypeName _ = "turtlesim/Spawn"

