-- Utility types for working with ROS.
module ROSTypes where
import Control.Applicative

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
                         , subConnected   :: Bool }

data PubStats = PubStats { bytesSent      :: Int
                         , numSent        :: Int
                         , pubDestination :: URI
                         , pubConnected   :: Bool }





