{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.RegionOfInterest where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Word as Word
import Foreign.Storable (Storable(..))
import qualified Ros.Internal.Util.StorableMonad as SM
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data RegionOfInterest = RegionOfInterest { _x_offset :: Word.Word32
                                         , _y_offset :: Word.Word32
                                         , _height :: Word.Word32
                                         , _width :: Word.Word32
                                         , _do_rectify :: P.Bool
                                         } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''RegionOfInterest)

instance RosBinary RegionOfInterest where
  put obj' = put (_x_offset obj') *> put (_y_offset obj') *> put (_height obj') *> put (_width obj') *> put (_do_rectify obj')
  get = RegionOfInterest <$> get <*> get <*> get <*> get <*> get

instance Storable RegionOfInterest where
  sizeOf _ = sizeOf (P.undefined::Word.Word32) +
             sizeOf (P.undefined::Word.Word32) +
             sizeOf (P.undefined::Word.Word32) +
             sizeOf (P.undefined::Word.Word32) +
             sizeOf (P.undefined::P.Bool)
  alignment _ = 8
  peek = SM.runStorable (RegionOfInterest <$> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek <*> SM.peek)
  poke ptr' obj' = SM.runStorable store' ptr'
    where store' = SM.poke (_x_offset obj') *> SM.poke (_y_offset obj') *> SM.poke (_height obj') *> SM.poke (_width obj') *> SM.poke (_do_rectify obj')

instance MsgInfo RegionOfInterest where
  sourceMD5 _ = "bdb633039d588fcccb441a4d43ccfe09"
  msgTypeName _ = "sensor_msgs/RegionOfInterest"

instance D.Default RegionOfInterest

