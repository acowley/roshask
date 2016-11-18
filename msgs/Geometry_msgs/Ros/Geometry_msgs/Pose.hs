{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Pose where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Ros.Geometry_msgs.Point as Point
import qualified Ros.Geometry_msgs.Quaternion as Quaternion
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Pose = Pose { _position :: Point.Point
                 , _orientation :: Quaternion.Quaternion
                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Pose)

instance RosBinary Pose where
  put obj' = put (_position obj') *> put (_orientation obj')
  get = Pose <$> get <*> get

instance Storable Pose where
  sizeOf _ = sizeOf (P.undefined::Point.Point) +
             sizeOf (P.undefined::Quaternion.Quaternion)
  alignment _ = 8
  peek = SM.runStorable (Pose <$> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_position obj') *> SM.poke (_orientation obj')

instance MsgInfo Pose where
  sourceMD5 _ = "e45d45a5a1ce597b249e23fb30fc871f"
  msgTypeName _ = "geometry_msgs/Pose"

instance D.Default Pose

