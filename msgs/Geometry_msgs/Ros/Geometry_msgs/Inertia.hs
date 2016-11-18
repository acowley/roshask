{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Inertia where
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

data Inertia = Inertia { _m :: P.Double
                       , _com :: Vector3.Vector3
                       , _ixx :: P.Double
                       , _ixy :: P.Double
                       , _ixz :: P.Double
                       , _iyy :: P.Double
                       , _iyz :: P.Double
                       , _izz :: P.Double
                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Inertia)

instance RosBinary Inertia where
  put obj' = put (_m obj') *> put (_com obj') *> put (_ixx obj') *> put (_ixy obj') *> put (_ixz obj') *> put (_iyy obj') *> put (_iyz obj') *> put (_izz obj')
  get = Inertia <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance Storable Inertia where
  sizeOf _ = sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::Vector3.Vector3) +
             sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double)
  alignment _ = 8
  peek = SM.runStorable (Inertia <$> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_m obj') *> SM.poke (_com obj') *> SM.poke (_ixx obj') *> SM.poke (_ixy obj') *> SM.poke (_ixz obj') *> SM.poke (_iyy obj') *> SM.poke (_iyz obj') *> SM.poke (_izz obj')

instance MsgInfo Inertia where
  sourceMD5 _ = "1d26e4bb6c83ff141c5cf0d883c2b0fe"
  msgTypeName _ = "geometry_msgs/Inertia"

instance D.Default Inertia

