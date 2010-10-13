{-# LANGUAGE PackageImports, MultiParamTypeClasses, ScopedTypeVariables, 
             FlexibleInstances #-}
-- |The primary entrypoint to the ROS client library portion of
-- roshask. This module defines the actions used to configure a ROS
-- Node.
module Ros.Node (Node, runNode, advertise, advertiseIO, advertiseBufferedIO, 
                 advertiseBuffered, subscribe, streamIO,
                 getShutdownAction, runHandler, getParam, getParam', liftIO,
                 module Ros.Core.RosTypes) where
import Control.Applicative ((<$>))
import Control.Concurrent (newEmptyMVar, readMVar, putMVar)
import Control.Concurrent.BoundedChan
import Control.Concurrent.STM (newTVarIO)
import Control.Monad (when)
import "monads-fd" Control.Monad.State (liftIO, get, put, execStateT)
import "monads-fd" Control.Monad.Reader (ask, asks, runReaderT)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Concurrent (forkIO, ThreadId)
import System.Environment (getEnvironment, getArgs)
import System.IO.Unsafe (unsafeInterleaveIO)
import Network.XmlRpc.Internals (XmlRpcType)
import Ros.Core.Msg.MsgInfo
import Ros.NodeType
import qualified Ros.ParameterServerAPI as P
import Ros.Core.RosBinary (RosBinary)
import Ros.Core.RosTypes
import Ros.RosTcp (subStream, runServer, runServerIO)
import qualified Ros.RunNode as RN
import Ros.TopicStats (recvMessageStat, sendMessageStat)
import Ros.Util.ArgRemapping

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
mkPub = mkPubAux (msgTypeName (undefined::a)) . runServer

mkPubIO :: forall a. (RosBinary a, MsgInfo a) =>
           Stream (IO a) -> Int -> IO Publication
mkPubIO = mkPubAux (msgTypeName (undefined::a)) . runServerIO

mkPubAux :: String -> ((URI -> Int -> IO ()) -> Int -> IO (IO (), Int)) ->
            Int -> IO Publication
mkPubAux trep runServer' bufferSize = 
    do stats <- newTVarIO M.empty
       (cleanup, port) <- runServer' (sendMessageStat stats) bufferSize
       known <- newTVarIO S.empty
       return $ Publication known trep port cleanup stats

-- |Subscribe to the given Topic. Returns the @Stream@ of values
-- received on over the Topic.
subscribe :: (RosBinary a, MsgInfo a) => TopicName -> Node (Stream a)
subscribe name = do n <- get
                    name' <- canonicalizeName =<< remapName name
                    let subs = subscriptions n
                    if M.member name' subs
                       then error $ "Already subscribed to "++name'
                       else do (stream, sub) <- liftIO (mkSub name')
                               put n { subscriptions = M.insert name' sub subs }
                               return stream

-- |Spin up a thread within a Node. This is typically used for message
-- handlers.
runHandler :: IO () -> Node ThreadId
runHandler = liftIO . forkIO

advertiseAux :: (Int -> IO Publication) -> Int -> TopicName -> Node ()
advertiseAux mkPub' bufferSize name = 
    do n <- get
       name' <- remapName =<< canonicalizeName name
       let pubs = publications n
       if M.member name' pubs
         then error $ "Already advertised " ++ name'
         else do pub <- liftIO $ mkPub' bufferSize
                 put n { publications = M.insert name' pub pubs }

-- |Advertise a Topic publishing a 'Stream' of pure values with a
-- per-client transmit buffer of the specified size.
advertiseBuffered :: (RosBinary a, MsgInfo a) => 
                     Int -> TopicName -> Stream a -> Node ()
advertiseBuffered bufferSize name s = advertiseAux (mkPub s) bufferSize name

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
advertiseBufferedIO bufferSize name s = advertiseAux (mkPubIO s) bufferSize name

-- |Get an action that will shutdown this Node.
getShutdownAction :: Node (IO ())
getShutdownAction = get >>= liftIO . readMVar . signalShutdown

-- |Apply any matching renames to a given name.
remapName :: String -> Node String
remapName name = asks (maybe name id . lookup name . snd)

-- |Convert relative names to absolute names. Leaves absolute names
-- unchanged.
canonicalizeName :: String -> Node String
canonicalizeName n@('/':_) = return n
canonicalizeName ('~':n) = do state <- get
                              let node = nodeName state
                              return $ node ++ "/" ++ n
canonicalizeName n = do (++n) . namespace <$> get

-- |Get a parameter value from the Parameter Server.
getServerParam :: XmlRpcType a => String -> Node (Maybe a)
getServerParam var = do state <- get
                        let masterUri = master state
                            myName = nodeName state
                        -- Call hasParam first because getParam only returns 
                        -- a partial result (just the return code) in failure.
                        hasParam <- liftIO $ P.hasParam masterUri myName var
                        case hasParam of
                          Right True -> liftIO $ P.getParam masterUri myName var
                          _ -> return Nothing

-- |Get the value associated with the given parameter name.
getParam :: (XmlRpcType a, FromParam a) => String -> Node (Maybe a)
getParam var = do var' <- remapName =<< canonicalizeName var
                  params <- fst <$> ask
                  case lookup var' params of
                    Just val -> return . Just $ fromParam val
                    Nothing -> getServerParam var'

-- |Get the value associated with the given parameter name. If the
-- parameter is not set, return the second argument as the default
-- value.
getParam' :: (XmlRpcType a, FromParam a) => String -> a -> Node a
getParam' var def = maybe def id <$> getParam var
                        
-- |Run a ROS Node.
runNode :: NodeName -> Node a -> IO ()
runNode name (Node n) = 
    do myURI <- newEmptyMVar
       sigStop <- newEmptyMVar
       env <- liftIO getEnvironment
       args <- liftIO getArgs
       let getConfig' var def = maybe def id $ lookup var env
           getConfig = flip lookup env
           master = getConfig' "ROS_MASTER_URI" "http://localhost:11311"
           namespace = let ns = getConfig' "ROS_NAMESPACE" "/"
                       in if last ns == '/' then ns else ns ++ "/"
           (nameMap, params) = parseRemappings args
           name' = case lookup "__name" params of
                     Just x -> fromParam x
                     Nothing -> case name of
                                  '/':_ -> name
                                  _ -> namespace ++ name
           -- Name remappings apply to exact strings and resolved names.
           resolve p@(('/':_),_) = [p]
           resolve (('_':n),v) = [(name'++"/"++n, v)]
           resolve (('~':n),v) = [(name'++"/"++ n, v)] --, ('_':n,v)]
           resolve (n,v) = [(namespace ++ n,v), (n,v)]
           nameMap' = concatMap resolve nameMap
           params' = concatMap resolve params
       when (not $ null nameMap')
            (putStrLn $ "Remapping name(s) "++show nameMap')
       when (not $ null params') 
            (putStrLn $ "Setting parameter(s) "++show params')
       case getConfig "ROS_IP" of
         Nothing -> case getConfig "ROS_HOSTNAME" of
                      Nothing -> return ()
                      Just n -> putMVar myURI $! "http://"++n
         Just ip -> putMVar myURI $! "http://"++ip
       go name' . execStateT (runReaderT n (params', nameMap')) $
         NodeState name' namespace master myURI sigStop M.empty M.empty
    where go name' ns = ns >>= RN.runNode name'
