{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.PointCloud2 where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import Ros.Internal.Msg.HeaderSupport
import qualified Data.Vector.Storable as V
import qualified Data.Word as Word
import qualified Ros.Sensor_msgs.PointField as PointField
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data PointCloud2 = PointCloud2 { _header :: Header.Header
                               , _height :: Word.Word32
                               , _width :: Word.Word32
                               , _fields :: [PointField.PointField]
                               , _is_bigendian :: P.Bool
                               , _point_step :: Word.Word32
                               , _row_step :: Word.Word32
                               , __data :: V.Vector Word.Word8
                               , _is_dense :: P.Bool
                               } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''PointCloud2)

instance RosBinary PointCloud2 where
  put obj' = put (_header obj') *> put (_height obj') *> put (_width obj') *> putList (_fields obj') *> put (_is_bigendian obj') *> put (_point_step obj') *> put (_row_step obj') *> put (__data obj') *> put (_is_dense obj')
  get = PointCloud2 <$> get <*> get <*> get <*> getList <*> get <*> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader PointCloud2 where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo PointCloud2 where
  sourceMD5 _ = "1158d486dd51d683ce2f1be655c3c181"
  msgTypeName _ = "sensor_msgs/PointCloud2"

instance D.Default PointCloud2

