{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.Image where
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
import qualified Ros.Std_msgs.Header as Header
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data Image = Image { _header :: Header.Header
                   , _height :: Word.Word32
                   , _width :: Word.Word32
                   , _encoding :: P.String
                   , _is_bigendian :: Word.Word8
                   , _step :: Word.Word32
                   , __data :: V.Vector Word.Word8
                   } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''Image)

instance RosBinary Image where
  put obj' = put (_header obj') *> put (_height obj') *> put (_width obj') *> put (_encoding obj') *> put (_is_bigendian obj') *> put (_step obj') *> put (__data obj')
  get = Image <$> get <*> get <*> get <*> get <*> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader Image where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo Image where
  sourceMD5 _ = "060021388200f6f0f447d0fcd9c64743"
  msgTypeName _ = "sensor_msgs/Image"

instance D.Default Image

