{-# LANGUAGE GADTs, ExistentialQuantification, ScopedTypeVariables #-}
-- ROS Node implementation.
module ROSTypes where
import Data.Map (Map)
import Data.Set (Set)
--import qualified Network.XmlRpc.Internals as XmlRpc
import Data.Typeable
import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan
import System.IO (Handle)

type URI       = String
type CallerID  = String
type TopicName = String

-- |A reflective interface for topics that names the type of data
-- transmitted over the topic and includes the MD5 hash of the .msg
-- file the type is derived from.
class TopicType t where
    name :: t -> String
    md5  :: t -> String

class Subscriber t where
    subscribeTo :: t -> URI -> IO ThreadId

class TopicType t => Publisher t where
    publishTo :: t -> (a -> IO ThreadId)

-- |A SubTopic has a function for connecting to a new publisher and the
-- set of all known publishers of this topic.
data SubTopic a where
    SubTopic :: Typeable a => (URI -> IO ThreadId) -> Set URI -> SubTopic a

instance Typeable a => Typeable (SubTopic a) where
    typeOf (SubTopic _ _) = mkTyConApp subCon [typeOf (undefined::a)]
        where subCon = mkTyCon "ROS.SubTopic"

-- |A PubTopic has a list of actions for pushing new data to subscribers.
data PubTopic a where
    PubTopic :: Typeable a => [a -> IO ()] -> PubTopic a

instance Typeable a => Typeable (PubTopic a) where
    typeOf (PubTopic _) = mkTyConApp pubCon [typeOf (undefined::a)]
        where pubCon = mkTyCon "ROS.PubTopic"

instance TopicType a => TopicType (SubTopic a) where
    name (SubTopic _ _) = name (undefined :: a)
    md5 (SubTopic _ _) = md5 (undefined :: a)

data ETopic = forall a. (TopicType a, Typeable a) => ETopic a

instance Typeable ETopic where
    typeOf (ETopic a) = typeOf a

data Node = Node { master        :: URI
                 , subscriptions :: Map TopicName ETopic
                 , publications  :: Map TopicName ETopic
                 , threads       :: [ThreadId]
                 , handles       :: [Handle] }

--, params :: Map String XmlRpc.Value }

