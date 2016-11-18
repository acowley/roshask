{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Twist where
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

data Twist = Twist { _linear :: Vector3.Vector3
                   , _angular :: Vector3.Vector3
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Twist)

instance RosBinary Twist where
  put obj' = put (_linear obj') *> put (_angular obj')
  get = Twist <$> get <*> get

instance Storable Twist where
  sizeOf _ = sizeOf (P.undefined::Vector3.Vector3) +
             sizeOf (P.undefined::Vector3.Vector3)
  alignment _ = 8
  peek = SM.runStorable (Twist <$> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_linear obj') *> SM.poke (_angular obj')

instance MsgInfo Twist where
  sourceMD5 _ = "9f195f881246fdfa2798d1d3eebca84a"
  msgTypeName _ = "geometry_msgs/Twist"

instance D.Default Twist

