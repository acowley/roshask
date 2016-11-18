{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Geometry_msgs.Transform where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Ros.Geometry_msgs.Quaternion as Quaternion
import qualified Ros.Geometry_msgs.Vector3 as Vector3
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Transform = Transform { _translation :: Vector3.Vector3
                           , _rotation :: Quaternion.Quaternion
                           } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Transform)

instance RosBinary Transform where
  put obj' = put (_translation obj') *> put (_rotation obj')
  get = Transform <$> get <*> get

instance Storable Transform where
  sizeOf _ = sizeOf (P.undefined::Vector3.Vector3) +
             sizeOf (P.undefined::Quaternion.Quaternion)
  alignment _ = 8
  peek = SM.runStorable (Transform <$> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_translation obj') *> SM.poke (_rotation obj')

instance MsgInfo Transform where
  sourceMD5 _ = "ac9eff44abf714214112b05d54a3cf9b"
  msgTypeName _ = "geometry_msgs/Transform"

instance D.Default Transform

