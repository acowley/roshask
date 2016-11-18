{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Std_msgs.Char where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Int as Int
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Char = Char { __data :: Int.Int8
                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Char)

instance RosBinary Char where
  put obj' = put (__data obj')
  get = Char <$> get

instance Storable Char where
  sizeOf _ = sizeOf (P.undefined::Int.Int8)
  alignment _ = 8
  peek = SM.runStorable (Char <$> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (__data obj')

instance MsgInfo Char where
  sourceMD5 _ = "1bf77f25acecdedba0e224b162199717"
  msgTypeName _ = "std_msgs/Char"

instance D.Default Char

