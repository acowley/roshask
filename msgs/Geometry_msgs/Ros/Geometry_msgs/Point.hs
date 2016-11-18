{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Point where
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

data Point = Point { _x :: P.Double
                   , _y :: P.Double
                   , _z :: P.Double
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Point)

instance RosBinary Point where
  put obj' = put (_x obj') *> put (_y obj') *> put (_z obj')
  get = Point <$> get <*> get <*> get

instance Storable Point where
  sizeOf _ = sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double)
  alignment _ = 8
  peek = SM.runStorable (Point <$> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_x obj') *> SM.poke (_y obj') *> SM.poke (_z obj')

instance MsgInfo Point where
  sourceMD5 _ = "4a842b65f413084dc2b10fb484ea7f17"
  msgTypeName _ = "geometry_msgs/Point"

instance D.Default Point

