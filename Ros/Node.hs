{-# LANGUAGE PackageImports, MultiParamTypeClasses, ScopedTypeVariables, 
             FlexibleInstances #-}
module Ros.Node (Node, runNode, advertise, advertiseIO, subscribe, streamIO,
                 getShutdownAction, runHandler, module Ros.RosTypes) where
import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent (MVar, newEmptyMVar, readMVar, putMVar)
import Control.Concurrent.BoundedChan
import Control.Concurrent.STM (atomically, TVar, readTVar, writeTVar, 
                               newTVarIO)
import "monads-fd" Control.Monad.State
import "monads-fd" Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Concurrent (forkIO, ThreadId)
import System.Environment (getEnvironment)
import System.IO.Unsafe (unsafeInterleaveIO)
import Msg.MsgInfo
import Ros.NodeType
import Ros.RosBinary (RosBinary)
import Ros.RosTypes
import Ros.RosTcp
import Ros.SlaveAPI (RosSlave(..))
import qualified Ros.RunNode as RN
import Ros.TopicStats

-- |Maximum number of items to buffer for a subscriber.
recvBufferSize :: Int
recvBufferSize = 10

-- |Spark a thread that funnels a Stream from a URI into the given
-- Chan.
addSource :: (RosBinary a, MsgInfo a) => 
             String -> (URI -> Int -> IO ()) -> BoundedChan a -> URI -> 
             IO ThreadId
addSource tname updateStats c uri = 
    forkIO $ subStream uri tname (updateStats uri) >>= go
    where go (Cons x xs) = writeChan c x >> go xs

-- Create a new Subscription value that will act as a named input
-- channel with zero or more connected publishers.
mkSub :: forall a. (RosBinary a, MsgInfo a) => 
         String -> IO (Stream a, Subscription)
mkSub tname = do c <- newBoundedChan recvBufferSize
                 stream <- list2stream <$> getChanContents c
                 known <- newTVarIO S.empty
                 stats <- newTVarIO M.empty
                 let topicType = msgTypeName (undefined::a)
                     updateStats = recvMessageStat stats
                     sub = Subscription known (addSource tname updateStats c) 
                                        topicType stats
                 return (stream, sub)
    where list2stream (x:xs) = Cons x (list2stream xs)
          list2stream [] = error "mkSub expected an infinite list"

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

-- |Convert a 'Stream' of 'IO' actions to a 'Stream' of pure values.
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

-- |Get a variable binding from the environment.
getConfig :: String -> Node (Maybe String)
getConfig = asks . (. fst) . lookup

-- |Get a variable binding from the environment. If the variable named
-- by the first parameter is not found, the supplied default value
-- will be returned.
getConfig' :: String -> String -> Node String
getConfig' var def = asks (maybe def id . lookup var . fst)

-- If the master URI is set in the ROS_MASTER_URI environment variable
-- then use that, otherwise use http://localhost:11311
findMaster :: [(String, String)] -> String
findMaster = maybe "http://localhost:11311" id . lookup "ROS_MASTER_URI"

-- |Get the namespace of this node.
getNamespace = getConfig' "ROS_NAMESPACE" "/"

-- |Resolve a name. If the name is absolute (begins with a forward
-- slash, '/'), it is returned unchanged. Otherwise, it is prefixed by
-- the current namespace.
resolveName :: String -> Node String
resolveName n@('/':_) = return n
resolveName name = (++ name) <$> getNamespace

-- |Apply any matching renames to a given name.
remapName :: String -> Node String
remapName name = asks (maybe name id . lookup name . snd)

-- |Run a ROS Node.
runNode :: NodeName -> Node a -> IO ()
runNode name (Node n) = 
    do myURI <- newEmptyMVar
       sigStop <- newEmptyMVar
       env <- liftIO getEnvironment
       let master = findMaster env
       go . execStateT (runReaderT n (env,[])) $
         NodeState name master myURI sigStop M.empty M.empty
    where go ns = ns >>= RN.runNode name
