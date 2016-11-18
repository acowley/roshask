{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.AccelWithCovariance where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Vector.Storable as V
import qualified Ros.Geometry_msgs.Accel as Accel
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data AccelWithCovariance = AccelWithCovariance { _accel :: Accel.Accel
                                               , _covariance :: V.Vector P.Double
                                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''AccelWithCovariance)

instance RosBinary AccelWithCovariance where
  put obj' = put (_accel obj') *> put (_covariance obj')
  get = AccelWithCovariance <$> get <*> get

instance Storable AccelWithCovariance where
  sizeOf _ = sizeOf (P.undefined::Accel.Accel) +
             36 * (sizeOf (P.undefined::P.Double))
  alignment _ = 8
  peek = SM.runStorable (AccelWithCovariance <$> SM.peek <*> V.replicateM 36 (SM.peek))
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_accel obj') *> V.mapM_ (SM.poke) (_covariance obj')

instance MsgInfo AccelWithCovariance where
  sourceMD5 _ = "ad5a718d699c6be72a02b8d6a139f334"
  msgTypeName _ = "geometry_msgs/AccelWithCovariance"

instance D.Default AccelWithCovariance

