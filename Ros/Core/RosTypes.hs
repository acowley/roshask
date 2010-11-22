-- |Utility types for working with ROS.
module Ros.Core.RosTypes (ROSTime, ROSDuration, URI, CallerID, TopicName, 
                          NodeName, ParamName, TopicType, ConnectionID) where
import Data.Word (Word32)

type URI          = String
type CallerID     = String
type TopicName    = String
type NodeName     = String
type ParamName    = String
type TopicType    = String
type ConnectionID = Int

-- |ROSTime is a tuple of (seconds, nanoseconds)
type ROSTime = (Word32, Word32)

-- |ROSDuration is a tuple of (seconds, nanoseconds)
type ROSDuration = (Word32, Word32)

