{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Point32 where
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

data Point32 = Point32 { _x :: P.Float
                       , _y :: P.Float
                       , _z :: P.Float
                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Point32)

instance RosBinary Point32 where
  put obj' = put (_x obj') *> put (_y obj') *> put (_z obj')
  get = Point32 <$> get <*> get <*> get

instance Storable Point32 where
  sizeOf _ = sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::P.Float)
  alignment _ = 8
  peek = SM.runStorable (Point32 <$> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_x obj') *> SM.poke (_y obj') *> SM.poke (_z obj')

instance MsgInfo Point32 where
  sourceMD5 _ = "cc153912f1453b708d221682bc23d9ac"
  msgTypeName _ = "geometry_msgs/Point32"

instance D.Default Point32

