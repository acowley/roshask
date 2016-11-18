{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.CompressedImage where
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

data CompressedImage = CompressedImage { _header :: Header.Header
                                       , _format :: P.String
                                       , __data :: V.Vector Word.Word8
                                       } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''CompressedImage)

instance RosBinary CompressedImage where
  put obj' = put (_header obj') *> put (_format obj') *> put (__data obj')
  get = CompressedImage <$> get <*> get <*> get
  putMsg = putStampedMsg

instance HasHeader CompressedImage where
  getSequence = view (header . Header.seq)
  getFrame    = view (header . Header.frame_id)
  getStamp    = view (header . Header.stamp)
  setSequence = set  (header . Header.seq)

instance MsgInfo CompressedImage where
  sourceMD5 _ = "8f7a12909da2c9d3332d540a0977563f"
  msgTypeName _ = "sensor_msgs/CompressedImage"

instance D.Default CompressedImage

