{-# LANGUAGE PackageImports #-}
module Node where
import Control.Concurrent.STM.TVar (TVar)
import "mtl" Control.Monad.State
import Data.Binary
import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable (Typeable, TypeRep)
import Control.Concurrent (ThreadId)
import ROSTypes

data Subscription = Subscription { knownPubs :: Set URI
                                 , addPub    :: URI -> IO ()
                                 , subType   :: TypeRep
                                 , subStats  :: TVar SubStats }

data Publication = Publication { subscribers :: Set URI
                               , pubType     :: TypeRep
                               , pubStats    :: TVar PubStats
                               , pubThread   :: ThreadId }

data Topic = Topic { inputs :: [TopicName]
                   , outputs :: [TopicName] }

data NodeState = NodeState { nodeName      :: String
                           , master        :: URI
                           , subscriptions :: Map String Subscription
                           , publications  :: Map String Publication }

newtype Node a = Node { unNode :: StateT NodeState IO a }

instance Monad Node where
    (Node s) >>= f = Node $ s >>= unNode . f
    return = Node . return

subscribe :: (Typeable a, Binary a) => TopicName -> Node (Stream a)
subscribe name = return undefined

advertise :: (Typeable a, Binary a) => TopicName -> Stream a -> Node ()
advertise name stream = return ()

-- | A filtered stream.
fooBar :: (Num a, Ord a) => Stream a -> Stream a
fooBar (Stream x xs) = if x < 2 then fooBar xs else Stream x (fooBar xs)

mkSub :: Typeable a => (Stream a -> b) -> Subscription
mkSub handler = undefined

mkPub :: Typeable a => Stream a -> Publication
mkPub gen = undefined

runNode :: NodeName -> 
           [(TopicName, Subscription)] -> 
           [(TopicName, Publication)] -> IO ()
runNode name subs pubs = return ()