{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.TwistWithCovariance where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Vector.Storable as V
import qualified Ros.Geometry_msgs.Twist as Twist
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data TwistWithCovariance = TwistWithCovariance { _twist :: Twist.Twist
                                               , _covariance :: V.Vector P.Double
                                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''TwistWithCovariance)

instance RosBinary TwistWithCovariance where
  put obj' = put (_twist obj') *> put (_covariance obj')
  get = TwistWithCovariance <$> get <*> get

instance Storable TwistWithCovariance where
  sizeOf _ = sizeOf (P.undefined::Twist.Twist) +
             36 * (sizeOf (P.undefined::P.Double))
  alignment _ = 8
  peek = SM.runStorable (TwistWithCovariance <$> SM.peek <*> V.replicateM 36 (SM.peek))
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_twist obj') *> V.mapM_ (SM.poke) (_covariance obj')

instance MsgInfo TwistWithCovariance where
  sourceMD5 _ = "1fe8a28e6890a4cc3ae4c3ca5c7d82e6"
  msgTypeName _ = "geometry_msgs/TwistWithCovariance"

instance D.Default TwistWithCovariance

