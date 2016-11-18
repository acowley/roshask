{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Turtlesim.Pose where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Pose = Pose { _x :: P.Float
                 , _y :: P.Float
                 , _theta :: P.Float
                 , _linear_velocity :: P.Float
                 , _angular_velocity :: P.Float
                 } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Pose)

instance RosBinary Pose where
  put obj' = put (_x obj') *> put (_y obj') *> put (_theta obj') *> put (_linear_velocity obj') *> put (_angular_velocity obj')
  get = Pose <$> get <*> get <*> get <*> get <*> get

instance Storable Pose where
  sizeOf _ = sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float)
  alignment _ = 8
  peek = SM.runStorable (Pose <$> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_x obj') *> SM.poke (_y obj') *> SM.poke (_theta obj') *> SM.poke (_linear_velocity obj') *> SM.poke (_angular_velocity obj')

instance MsgInfo Pose where
  sourceMD5 _ = "863b248d5016ca62ea2e895ae5265cf9"
  msgTypeName _ = "turtlesim/Pose"

instance D.Default Pose

