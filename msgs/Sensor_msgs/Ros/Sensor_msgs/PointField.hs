{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Ros.Sensor_msgs.PointField where
import qualified Prelude as P
import Prelude ((.), (+), (*))
import qualified Data.Typeable as T
import Control.Applicative
import Ros.Internal.RosBinary
import Ros.Internal.Msg.MsgInfo
import qualified GHC.Generics as G
import qualified Data.Default.Generics as D
import qualified Data.Word as Word
import Lens.Family.TH (makeLenses)
import Lens.Family (view, set)

data PointField = PointField { _name :: P.String
                             , _offset :: Word.Word32
                             , _datatype :: Word.Word8
                             , _count :: Word.Word32
                             } deriving (P.Show, P.Eq, P.Ord, T.Typeable, G.Generic)

$(makeLenses ''PointField)

instance RosBinary PointField where
  put obj' = put (_name obj') *> put (_offset obj') *> put (_datatype obj') *> put (_count obj')
  get = PointField <$> get <*> get <*> get <*> get

instance MsgInfo PointField where
  sourceMD5 _ = "268eacb2962780ceac86cbd17e328150"
  msgTypeName _ = "sensor_msgs/PointField"

instance D.Default PointField

int8 :: Word.Word8
int8 = 1

uint8 :: Word.Word8
uint8 = 2

int16 :: Word.Word8
int16 = 3

uint16 :: Word.Word8
uint16 = 4

int32 :: Word.Word8
int32 = 5

uint32 :: Word.Word8
uint32 = 6

float32 :: Word.Word8
float32 = 7

float64 :: Word.Word8
float64 = 8

