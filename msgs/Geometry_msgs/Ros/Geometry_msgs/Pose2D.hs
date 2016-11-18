{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Pose2D where
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

data Pose2D = Pose2D { _x :: P.Double
                     , _y :: P.Double
                     , _theta :: P.Double
                     } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Pose2D)

instance RosBinary Pose2D where
  put obj' = put (_x obj') *> put (_y obj') *> put (_theta obj')
  get = Pose2D <$> get <*> get <*> get

instance Storable Pose2D where
  sizeOf _ = sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double)
  alignment _ = 8
  peek = SM.runStorable (Pose2D <$> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_x obj') *> SM.poke (_y obj') *> SM.poke (_theta obj')

instance MsgInfo Pose2D where
  sourceMD5 _ = "938fa65709584ad8e77d238529be13b8"
  msgTypeName _ = "geometry_msgs/Pose2D"

instance D.Default Pose2D

