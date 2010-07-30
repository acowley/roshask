{-# LANGUAGE PackageImports, MultiParamTypeClasses, ScopedTypeVariables #-}
module Ros.Node (Node, runNode, advertise, advertiseIO, subscribe, 
                 runHandler, module Ros.RosTypes) where
import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent (MVar, newEmptyMVar, readMVar, putMVar)
import Control.Concurrent.STM (atomically, STM, TVar, readTVar, writeTVar, 
                               newTVarIO)
import "monads-fd" Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Concurrent (forkIO, ThreadId)
import System.Environment (getEnvironment)
import System.IO.Unsafe (unsafeInterleaveIO)
import Msg.MsgInfo
import Ros.RosBinary (RosBinary)
import Ros.RosTypes
import Ros.RosTcp
import Ros.SlaveAPI (RosSlave(..))
import qualified Ros.RunNode as RN
import Ros.TopicStats
--import Ros.Util.RingChan
import Control.Concurrent.BoundedChan

type RingChan = BoundedChan
newRingChan = newBoundedChan

data Subscription = Subscription { knownPubs :: TVar (Set URI)
                                 , addPub    :: URI -> IO ThreadId
                                 , subType   :: String
                                 , subStats  :: StatMap SubStats }

data Publication = Publication { subscribers :: TVar (Set URI)
                               , pubType     :: String
                               , pubPort     :: Int
                               , pubCleanup  :: IO ()
                               , pubStats    :: StatMap PubStats }

data NodeState = NodeState { nodeName       :: String
                           , master         :: URI
                           , nodeURI        :: MVar URI
                           , signalShutdown :: MVar (IO ())
                           , subscriptions  :: Map String Subscription
                           , publications   :: Map String Publication }

newtype Node a = Node { unNode :: StateT NodeState IO a }

instance Functor Node where
    fmap f (Node s) = Node (fmap f s)

instance Applicative Node where
    pure = Node . pure
    Node f <*> Node x = Node (f <*> x)

instance Monad Node where
    (Node s) >>= f = Node $ s >>= unNode . f
    return = Node . return

instance MonadIO Node where
    liftIO m = Node $ liftIO m

instance MonadState NodeState Node where
    get = Node get
    put = Node . put

instance RosSlave NodeState where
    getMaster = master
    getNodeName = nodeName
    getNodeURI = nodeURI
    getSubscriptions = atomically . mapM formatSub . M.toList . subscriptions
        where formatSub (name, sub) = let topicType = subType sub
                                      in do stats <- readTVar (subStats sub)
                                            stats' <- mapM statSnapshot . 
                                                      M.toList $
                                                      stats
                                            return (name, topicType, stats')
    getPublications = atomically . mapM formatPub . M.toList . publications
        where formatPub (name, pub) = let topicType = pubType pub
                                      in do stats <- readTVar (pubStats pub)
                                            stats' <- mapM statSnapshot .
                                                      M.toList $
                                                      stats
                                            return (name, topicType, stats')
    publisherUpdate ns name uris = 
        let act = join.atomically $
                  case M.lookup name (subscriptions ns) of
                    Nothing -> return (return ())
                    Just sub -> do let add = addPub sub >=> \_ -> return ()
                                   known <- readTVar (knownPubs sub) 
                                   (act,known') <- foldM (connectToPub add)
                                                         (return (), known)
                                                         uris
                                   writeTVar (knownPubs sub) known'
                                   return act
        in act
    getTopicPortTCP = ((pubPort <$> ) .) . flip M.lookup . publications
    setShutdownAction ns a = putMVar (signalShutdown ns) a
    stopNode = mapM_ (pubCleanup . snd) . M.toList . publications

-- If a given URI is not a part of a Set of known URIs, add an action
-- to effect a subscription to an accumulated action and add the URI
-- to the Set.
connectToPub :: Monad m => 
                (URI -> IO ()) -> (IO (), Set URI) -> URI -> m (IO (), Set URI)
connectToPub doSub (act, known) uri = if S.member uri known
                                      then return (act, known)
                                      else let known' = S.insert uri known
                                           in return (doSub uri >> act, known')

-- |Maximum number of items to buffer for a subscriber.
recvBufferSize :: Int
recvBufferSize = 10

-- |Spark a thread that funnels a Stream from a URI into the given
-- Chan.
addSource :: (RosBinary a, MsgInfo a) => 
             String -> (URI -> Int -> IO ()) -> RingChan a -> URI -> 
             IO ThreadId
addSource tname updateStats c uri = 
    forkIO $ subStream uri tname (updateStats uri) >>= go
    where go (Cons x xs) = writeChan c x >> go xs

-- Create a new Subscription value that will act as a named input
-- channel with zero or more connected publishers.
mkSub :: forall a. (RosBinary a, MsgInfo a) => 
         String -> IO (Stream a, Subscription)
mkSub tname = do c <- newRingChan recvBufferSize
                 stream <- list2stream <$> getChanContents c
                 known <- newTVarIO S.empty
                 stats <- newTVarIO M.empty
                 let topicType = msgTypeName (undefined::a)
                     updateStats = recvMessageStat stats
                     sub = Subscription known (addSource tname updateStats c) 
                                        topicType stats
                 return (stream, sub)
    where list2stream (x:xs) = Cons x (list2stream xs)

mkPub :: forall a. (RosBinary a, MsgInfo a) => 
         Stream a -> Int -> IO Publication
mkPub s bufferSize = 
    do stats <- newTVarIO M.empty
       (cleanup, port) <- runServer s (sendMessageStat stats) bufferSize
       known <- newTVarIO S.empty
       let trep = msgTypeName (undefined::a)
       return $ Publication known trep port cleanup stats

-- |Subscribe to the given Topic. Returns the @Stream@ of values
-- received on over the Topic.
subscribe :: (RosBinary a, MsgInfo a) => TopicName -> Node (Stream a)
subscribe name = do n <- get
                    let subs = subscriptions n
                    if M.member name subs
                       then error $ "Already subscribed to "++name
                       else do (stream, sub) <- liftIO (mkSub name)
                               put n { subscriptions = M.insert name sub subs }
                               return stream

-- |Spin up a thread within a Node. This is typically used for message
-- handlers.
runHandler :: IO () -> Node ThreadId
runHandler = liftIO . forkIO

-- |Advertise a Topic publishing a 'Stream' of pure values with a
-- per-client transmit buffer of the specified size.
advertiseBuffered :: (RosBinary a, MsgInfo a) => 
                     Int -> TopicName -> Stream a -> Node ()
advertiseBuffered bufferSize name stream = 
    do n <- get
       let pubs = publications n
       if M.member name pubs 
         then error $ "Already advertised "++name
         else do pub <- liftIO $ mkPub stream bufferSize
                 put n { publications = M.insert name pub pubs }


-- |Advertise a Topic publishing a 'Stream' of pure values.
advertise :: (RosBinary a, MsgInfo a) => TopicName -> Stream a -> Node ()
advertise = advertiseBuffered 1

streamIO :: Stream (IO a) -> IO (Stream a)
streamIO (Cons x xs) = unsafeInterleaveIO $
                       do x' <- x
                          xs' <- streamIO xs
                          return $ Cons x' xs'

-- |Advertise a Topic publishing a 'Stream' of 'IO' values.
advertiseIO :: (RosBinary a, MsgInfo a) => 
               TopicName -> Stream (IO a) -> Node ()
advertiseIO = advertiseBufferedIO 1

-- |Advertise a Topic publishing a 'Stream' of 'IO' values with a
-- per-client transmit buffer of the specified size.
advertiseBufferedIO :: (RosBinary a, MsgInfo a) =>
                       Int -> TopicName -> Stream (IO a) -> Node ()
advertiseBufferedIO bufferSize name stream = 
    advertiseBuffered bufferSize name =<< liftIO (streamIO stream)

-- |Get an action that will shutdown this Node.
getShutdownAction :: Node (IO ())
getShutdownAction = get >>= liftIO . readMVar . signalShutdown

-- If the master URI is set in the ROS_MASTER_URI environment variable
-- then use that, otherwise use http://localhost:11311
findMaster :: IO String
findMaster = do env <- getEnvironment
                case lookup "ROS_MASTER_URI" env of
                  Just uri -> return uri
                  Nothing -> return "http://localhost:11311"

-- |Run a ROS Node.
runNode :: NodeName -> Node a -> IO ()
runNode name (Node n) = 
    do master <- findMaster
       myURI <- newEmptyMVar
       sigStop <- newEmptyMVar
       go $ execStateT n (NodeState name master myURI sigStop M.empty M.empty)
    where go ns = ns >>= RN.runNode name
