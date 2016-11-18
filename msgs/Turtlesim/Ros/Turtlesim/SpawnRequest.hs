{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.SpawnRequest where
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

data SpawnRequest = SpawnRequest { _x :: P.Float
                                 , _y :: P.Float
                                 , _theta :: P.Float
                                 , _name :: P.String
                                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''SpawnRequest)

instance RosBinary SpawnRequest where
  put obj' = put (_x obj') *> put (_y obj') *> put (_theta obj') *> put (_name obj')
  get = SpawnRequest <$> get <*> get <*> get <*> get

instance MsgInfo SpawnRequest where
  sourceMD5 _ = "57f001c49ab7b11d699f8606c1f4f7ff"
  msgTypeName _ = "turtlesim/SpawnRequest"

instance D.Default SpawnRequest

instance SrvInfo SpawnRequest where
  srvMD5 _ = "0b2d2e872a8e2887d5ed626f2bf2c561"
  srvTypeName _ = "turtlesim/Spawn"

