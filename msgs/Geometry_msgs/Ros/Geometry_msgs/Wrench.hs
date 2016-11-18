{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Wrench where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Ros.Geometry_msgs.Vector3 as Vector3
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Wrench = Wrench { _force :: Vector3.Vector3
                     , _torque :: Vector3.Vector3
                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Wrench)

instance RosBinary Wrench where
  put obj' = put (_force obj') *> put (_torque obj')
  get = Wrench <$> get <*> get

instance Storable Wrench where
  sizeOf _ = sizeOf (P.undefined::Vector3.Vector3) +
             sizeOf (P.undefined::Vector3.Vector3)
  alignment _ = 8
  peek = SM.runStorable (Wrench <$> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_force obj') *> SM.poke (_torque obj')

instance MsgInfo Wrench where
  sourceMD5 _ = "4f539cf138b23283b520fd271b567936"
  msgTypeName _ = "geometry_msgs/Wrench"

instance D.Default Wrench

