{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.SetMapResponse where
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

data SetMapResponse = SetMapResponse { _success :: P.Bool
                                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''SetMapResponse)

instance RosBinary SetMapResponse where
  put obj' = put (_success obj')
  get = SetMapResponse <$> get

instance Storable SetMapResponse where
  sizeOf _ = sizeOf (P.undefined::P.Bool)
  alignment _ = 8
  peek = SM.runStorable (SetMapResponse <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_success obj')

instance MsgInfo SetMapResponse where
  sourceMD5 _ = "358e233cde0c8a8bcfea4ce193f8fc15"
  msgTypeName _ = "nav_msgs/SetMapResponse"

instance D.Default SetMapResponse

instance SrvInfo SetMapResponse where
  srvMD5 _ = "c36922319011e63ed7784112ad4fdd32"
  srvTypeName _ = "nav_msgs/SetMap"

