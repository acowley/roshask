{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Bool where
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

data Bool = Bool { __data :: P.Bool
                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Bool)

instance RosBinary Bool where
  put obj' = put (__data obj')
  get = Bool <$> get

instance Storable Bool where
  sizeOf _ = sizeOf (P.undefined::P.Bool)
  alignment _ = 8
  peek = SM.runStorable (Bool <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Bool where
  sourceMD5 _ = "8b94c1b53db61fb6aed406028ad6332a"
  msgTypeName _ = "std_msgs/Bool"

instance D.Default Bool

