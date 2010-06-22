-- |ROS message types.
module MsgTypes where
import Data.ByteString (ByteString)

data MsgType = RBool | RInt8 | RUInt8 | RInt16 | RUInt16
             | RInt32 | RUInt32 | RInt64 | RUInt64
             | RFloat32 | RFloat64 | RString | RTime | RDuration
             | RFixedArray Int MsgType | RVarArray MsgType
             | RUserType ByteString
               deriving (Show, Eq, Ord)

data Msg = Msg String [(ByteString, MsgType)] deriving Show

