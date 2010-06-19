-- Utility types for working with ROS.
module ROSTypes where
import Control.Applicative
import Data.Word

type URI       = String
type CallerID  = String
type TopicName = String
type NodeName = String
type TopicType = String
type ConnectionID = Int

data Stream a = Stream a (Stream a)

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (Stream x xs) = x : takeS (n - 1) xs

instance Functor Stream where
    fmap f (Stream x xs) = Stream (f x) (fmap f xs)

instance Applicative Stream where
    pure x = Stream x (pure x)
    (Stream f fs) <*> (Stream x xs) = Stream (f x) (fs <*> xs)

data SubStats = SubStats { bytesReceived  :: Int
                         , subDestination :: URI
                         , subConnected   :: Bool }

data PubStats = PubStats { bytesSent      :: Int
                         , numSent        :: Int
                         , pubConnected   :: Bool }

-- ROSTime is a tuple of (seconds, nanoseconds)
type ROSTime = (Word32, Word32)

-- ROSDuration is a tuple of (seconds, nanoseconds)
type ROSDuration = (Word32, Word32)






