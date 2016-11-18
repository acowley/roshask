{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Nav_msgs.MapMetaData where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.RosTypes
import qualified Data.Word as Word
import qualified Ros.Geometry_msgs.Pose as Pose
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data MapMetaData = MapMetaData { _map_load_time :: ROSTime
                               , _resolution :: P.Float
                               , _width :: Word.Word32
                               , _height :: Word.Word32
                               , _origin :: Pose.Pose
                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''MapMetaData)

instance RosBinary MapMetaData where
  put obj' = put (_map_load_time obj') *> put (_resolution obj') *> put (_width obj') *> put (_height obj') *> put (_origin obj')
  get = MapMetaData <$> get <*> get <*> get <*> get <*> get

instance Storable MapMetaData where
  sizeOf _ = sizeOf (P.undefined::ROSTime) +
             sizeOf (P.undefined::P.Float) +
             sizeOf (P.undefined::Word.Word32) +
             sizeOf (P.undefined::Word.Word32) +
             sizeOf (P.undefined::Pose.Pose)
  alignment _ = 8
  peek = SM.runStorable (MapMetaData <$> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_map_load_time obj') *> SM.poke (_resolution obj') *> SM.poke (_width obj') *> SM.poke (_height obj') *> SM.poke (_origin obj')

instance MsgInfo MapMetaData where
  sourceMD5 _ = "10cfc8a2818024d3248802c00c95f11b"
  msgTypeName _ = "nav_msgs/MapMetaData"

instance D.Default MapMetaData

