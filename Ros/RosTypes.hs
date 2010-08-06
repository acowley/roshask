-- |Utility types for working with ROS.
module Ros.RosTypes (ROSTime, ROSDuration, URI, CallerID, TopicName, NodeName, 
                     TopicType, ConnectionID, Stream(..)) where
import Ros.Stream (Stream(..))
import Data.Word (Word32)

type URI       = String
type CallerID  = String
type TopicName = String
type NodeName = String
type TopicType = String
type ConnectionID = Int

-- |ROSTime is a tuple of (seconds, nanoseconds)
type ROSTime = (Word32, Word32)

-- |ROSDuration is a tuple of (seconds, nanoseconds)
type ROSDuration = (Word32, Word32)

