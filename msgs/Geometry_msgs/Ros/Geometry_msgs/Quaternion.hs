{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Quaternion where
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

data Quaternion = Quaternion { _x :: P.Double
                             , _y :: P.Double
                             , _z :: P.Double
                             , _w :: P.Double
                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Quaternion)

instance RosBinary Quaternion where
  put obj' = put (_x obj') *> put (_y obj') *> put (_z obj') *> put (_w obj')
  get = Quaternion <$> get <*> get <*> get <*> get

instance Storable Quaternion where
  sizeOf _ = sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double) +
             sizeOf (P.undefined::P.Double)
  alignment _ = 8
  peek = SM.runStorable (Quaternion <$> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_x obj') *> SM.poke (_y obj') *> SM.poke (_z obj') *> SM.poke (_w obj')

instance MsgInfo Quaternion where
  sourceMD5 _ = "a779879fadf0160734f906b8c19c7004"
  msgTypeName _ = "geometry_msgs/Quaternion"

instance D.Default Quaternion

