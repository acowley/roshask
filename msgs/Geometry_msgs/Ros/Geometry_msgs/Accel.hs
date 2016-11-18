{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Accel where
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

data Accel = Accel { _linear :: Vector3.Vector3
                   , _angular :: Vector3.Vector3
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Accel)

instance RosBinary Accel where
  put obj' = put (_linear obj') *> put (_angular obj')
  get = Accel <$> get <*> get

instance Storable Accel where
  sizeOf _ = sizeOf (P.undefined::Vector3.Vector3) +
             sizeOf (P.undefined::Vector3.Vector3)
  alignment _ = 8
  peek = SM.runStorable (Accel <$> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_linear obj') *> SM.poke (_angular obj')

instance MsgInfo Accel where
  sourceMD5 _ = "9f195f881246fdfa2798d1d3eebca84a"
  msgTypeName _ = "geometry_msgs/Accel"

instance D.Default Accel

