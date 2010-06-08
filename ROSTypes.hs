-- Utility types for working with ROS.
module ROSTypes where

type URI       = String
type CallerID  = String
type TopicName = String
type NodeName = String
type TopicType = String
type ConnectionID = Int

data Stream a = Stream a (Stream a)

data SubStats = SubStats { bytesReceived  :: Int
                         , subDestination :: URI
                         , subConnected   :: Bool }

data PubStats = PubStats { bytesSent      :: Int
                         , numSent        :: Int
                         , pubDestination :: URI
                         , pubConnected   :: Bool }


