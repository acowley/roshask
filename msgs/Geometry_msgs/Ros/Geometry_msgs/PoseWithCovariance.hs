{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.PoseWithCovariance where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Vector.Storable as V
import qualified Ros.Geometry_msgs.Pose as Pose
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data PoseWithCovariance = PoseWithCovariance { _pose :: Pose.Pose
                                             , _covariance :: V.Vector P.Double
                                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''PoseWithCovariance)

instance RosBinary PoseWithCovariance where
  put obj' = put (_pose obj') *> put (_covariance obj')
  get = PoseWithCovariance <$> get <*> get

instance Storable PoseWithCovariance where
  sizeOf _ = sizeOf (P.undefined::Pose.Pose) +
             36 * (sizeOf (P.undefined::P.Double))
  alignment _ = 8
  peek = SM.runStorable (PoseWithCovariance <$> SM.peek <*> V.replicateM 36 (SM.peek))
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_pose obj') *> V.mapM_ (SM.poke) (_covariance obj')

instance MsgInfo PoseWithCovariance where
  sourceMD5 _ = "c23e848cf1b7533a8d7c259073a97e6f"
  msgTypeName _ = "geometry_msgs/PoseWithCovariance"

instance D.Default PoseWithCovariance

