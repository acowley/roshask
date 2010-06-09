{-# LANGUAGE PackageImports, MultiParamTypeClasses, ScopedTypeVariables #-}
module Node (Node, runNode, advertise, subscribe) where
import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import "mtl" Control.Monad.State
import Data.Binary (Binary)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable, TypeRep, typeOf)
import Control.Concurrent (forkIO, ThreadId)
import ROSTypes
import RosTcp

data Subscription = Subscription { knownPubs :: Set URI
                                 , addPub    :: URI -> IO ThreadId
                                 , subType   :: TypeRep
                                 , subStats  :: TVar (Map URI SubStats) }

data Publication = Publication { subscribers :: Set URI
                               , pubType     :: TypeRep
                               , pubStats    :: TVar PubStats
                               , pubThread   :: ThreadId }

data NodeState = NodeState { nodeName      :: String
                           , master        :: URI
                           , subscriptions :: Map String Subscription
                           , publications  :: Map String Publication }

addSource :: Binary a => Chan a -> URI -> IO ThreadId
addSource c uri = do s <- connect uri
                     forkIO $ go s
    where go (Stream x xs) = writeChan c x >> go xs

-- Create a new Subscription value that will act as a named input
-- channel with zero or more connected publishers.
mkSub :: forall a. (Typeable a, Binary a) => IO (Stream a, Subscription)
mkSub = do c <- newChan
           stats <- newTVarIO M.empty
           stream <- list2stream <$> getChanContents c
           let sub = Subscription (S.empty)
                                  (addSource c)
                                  (typeOf (undefined::a))
                                  stats
           return (stream, sub)
    where list2stream (x:xs) = Stream x (list2stream xs)

newtype Node a = Node { unNode :: StateT NodeState IO a }

instance Monad Node where
    (Node s) >>= f = Node $ s >>= unNode . f
    return = Node . return

instance MonadIO Node where
    liftIO m = Node $ liftIO m

instance MonadState NodeState Node where
    get = Node get
    put = Node . put

subscribe :: (Typeable a, Binary a) => TopicName -> Node (Stream a)
subscribe name = do n <- get
                    let subs = subscriptions n
                    if M.member name subs
                       then error $ "Already subscribed to "++name
                       else do (stream, sub) <- liftIO mkSub
                               put n { subscriptions = M.insert name sub subs }
                               return stream

advertise :: (Typeable a, Binary a) => TopicName -> Stream a -> Node ()
advertise name stream = return ()

-- | A filtered stream.
fooBar :: (Num a, Ord a) => Stream a -> Stream a
fooBar (Stream x xs) = if x < 2 then fooBar xs else Stream x (fooBar xs)
          

mkPub :: Typeable a => Stream a -> Publication
mkPub gen = undefined

runNode :: NodeName -> 
           [(TopicName, Subscription)] -> 
           [(TopicName, Publication)] -> IO ()
runNode name subs pubs = return ()