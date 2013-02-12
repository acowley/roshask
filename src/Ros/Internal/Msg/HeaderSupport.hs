-- |If a message type's first field is of type Header, its sequence
-- number is automatically incremented by the ROS Topic machinery.
module Ros.Internal.Msg.HeaderSupport where
import Data.Binary (Put)
import Data.Word (Word32)
import Ros.Internal.RosBinary (RosBinary, put)
import Ros.Internal.RosTypes (ROSTime)

class HasHeader a where
    getSequence :: a -> Word32
    getFrame    :: a -> String
    getStamp    :: a -> ROSTime
    setSequence :: Word32 -> a -> a

-- |Serialize a message after setting the sequence number in its
-- header.
putStampedMsg :: (HasHeader a, RosBinary a) => Word32 -> a -> Put
putStampedMsg n v = put $ setSequence n v