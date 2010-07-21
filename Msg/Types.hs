-- |ROS message types.
module Msg.Types where
import Data.ByteString (ByteString)

-- |A variant type describing the types that may be included in a ROS
-- message.
data MsgType = RBool | RInt8 | RUInt8 | RInt16 | RUInt16
             | RInt32 | RUInt32 | RInt64 | RUInt64
             | RFloat32 | RFloat64 | RString | RTime | RDuration
             | RFixedArray Int MsgType | RVarArray MsgType
             | RUserType ByteString
               deriving (Show, Eq, Ord)

-- |A message has a name, an md5 sum, and a list of named, typed
-- fields.
data Msg = Msg String ByteString [(ByteString, MsgType)] deriving Show
