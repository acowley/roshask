{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
-- |The primary entrypoint to the ROS client library portion of
-- roshask. This module defines the actions used to configure a ROS
-- Node.
module Ros.Node (Node, runNode, advertise, advertiseBuffered, 
                 subscribe, getShutdownAction, runHandler, getParam, 
                 getParamOpt, getName, getNamespace, 
                 module Ros.Internal.RosTypes, Topic(..), topicRate, 
                 module Ros.Internal.RosTime, liftIO) where
import Control.Applicative ((<$>))
import Control.Concurrent (newEmptyMVar, readMVar, putMVar)
import Control.Concurrent.BoundedChan
import Control.Concurrent.STM (newTVarIO)
import Control.Monad (when)
import Control.Monad.State (liftIO, get, put, execStateT)
import Control.Monad.Reader (ask, asks, runReaderT)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Concurrent (forkIO, ThreadId)
import Data.Dynamic
import System.Environment (getEnvironment, getArgs)
import Network.XmlRpc.Internals (XmlRpcType)

import Ros.Internal.Msg.MsgInfo
import Ros.Internal.RosBinary (RosBinary)
import Ros.Internal.RosTypes
import Ros.Internal.RosTime
import Ros.Internal.Util.AppConfig (Config, parseAppConfig, forkConfig, configured)
import Ros.Internal.Util.ArgRemapping
import Ros.Node.Type
import qualified Ros.Graph.ParameterServer as P
import Ros.Node.RosTcp (subStream, runServer)
import qualified Ros.Node.RunNode as RN
import Ros.Topic
import Ros.Topic.Stats (recvMessageStat, sendMessageStat)
import Ros.Topic.Util (topicRate, share)

-- |Maximum number of items to buffer for a subscriber.
recvBufferSize :: Int
recvBufferSize = 10

-- |Spark a thread that funnels a Stream from a URI into the given
-- Chan.
addSource :: (RosBinary a, MsgInfo a) => 
             String -> (URI -> Int -> IO ()) -> BoundedChan a -> URI -> 
             Config ThreadId
addSource tname updateStats c uri = 
    forkConfig $ subStream uri tname (updateStats uri) >>= 
                 liftIO . forever . join . fmap (writeChan c)

-- Create a new Subscription value that will act as a named input
-- channel with zero or more connected publishers.
mkSub :: forall a. (RosBinary a, MsgInfo a) => 
         String -> Config (Topic IO a, Subscription)
mkSub tname = do c <- liftIO $ newBoundedChan recvBufferSize
                 let stream = Topic $ do x <- readChan c
                                         return (x, stream)
                 known <- liftIO $ newTVarIO S.empty
                 stats <- liftIO $ newTVarIO M.empty
                 r <- ask
                 let topicType = msgTypeName (undefined::a)
                     updateStats = recvMessageStat stats
                     addSource' = flip runReaderT r . addSource tname updateStats c
                     sub = Subscription known addSource' topicType stats
                 return (stream, sub)

mkPub :: forall a. (RosBinary a, MsgInfo a, Typeable a) => 
         Topic IO a -> Int -> Config Publication
mkPub t n = do t' <- liftIO $ share t
               mkPubAux (msgTypeName (undefined::a)) t' (runServer t') n

mkPubAux :: Typeable a => 
            String -> Topic IO a -> 
            ((URI -> Int -> IO ()) -> Int -> Config (Config (), Int)) ->
            Int -> Config Publication
mkPubAux trep t runServer' bufferSize = 
    do stats <- liftIO $ newTVarIO M.empty
       (cleanup, port) <- runServer' (sendMessageStat stats) bufferSize
       known <- liftIO $ newTVarIO S.empty
       cleanup' <- configured cleanup
       return $ Publication known trep port cleanup' (DynTopic t) stats

-- |Subscribe to the given Topic. Returns a 'Ros.TopicUtil.share'd 'Topic'.
subscribe :: (RosBinary a, MsgInfo a, Typeable a) => 
             TopicName -> Node (Topic IO a)
subscribe name = do n <- get
                    name' <- canonicalizeName =<< remapName name
                    r <- nodeAppConfig <$> ask
                    let subs = subscriptions n
                    when (M.member name' subs) 
                         (error $ "Already subscribed to "++name')
                    let pubs = publications n
                    if M.member name' pubs
                      then return . fromDynErr . pubTopic $ pubs M.! name'
                      else do (stream, sub) <- liftIO $
                                               runReaderT (mkSub name') r
                              put n { subscriptions = M.insert name' sub subs }
                              --return stream
                              liftIO $ share stream
  where fromDynErr = maybe (error msg) id . fromDynTopic
        msg = "Subscription to "++name++" at a different type than "++
              "what that Topic was already advertised at by this Node."

-- |Spin up a thread within a Node. This is typically used for message
-- handlers. Note that the supplied 'Topic' is traversed solely for
-- any side effects of its steps; the produced values are ignored.
runHandler :: (a -> IO b) -> Topic IO a -> Node ThreadId
runHandler = ((liftIO . forkIO . forever . join) .) . fmap

advertiseAux :: (Int -> Config Publication) -> Int -> TopicName -> Node ()
advertiseAux mkPub' bufferSize name = 
    do n <- get
       name' <- remapName =<< canonicalizeName name
       r <- nodeAppConfig <$> ask
       let pubs = publications n
       if M.member name' pubs
         then error $ "Already advertised " ++ name'
         else do pub <- liftIO $ runReaderT (mkPub' bufferSize) r
                 put n { publications = M.insert name' pub pubs }

-- |Advertise a 'Topic' publishing a stream of 'IO' values with a
-- per-client transmit buffer of the specified size.
advertiseBuffered :: (RosBinary a, MsgInfo a, Typeable a) => 
                     Int -> TopicName -> Topic IO a -> Node ()
advertiseBuffered bufferSize name s = advertiseAux (mkPub s) bufferSize name

-- |Advertise a 'Topic' publishing a stream of values produced in
-- the 'IO' monad.
advertise :: (RosBinary a, MsgInfo a, Typeable a) => 
             TopicName -> Topic IO a -> Node ()
advertise = advertiseBuffered 1

-- -- |Existentially quantified message type that roshask can
-- -- serialize. This type provides a way to work with collections of
-- -- differently typed 'Topic's.
-- data SomeMsg = forall a. (RosBinary a, MsgInfo a, Typeable a) => SomeMsg a

-- -- |Advertise projections of a 'Topic' as discrete 'Topic's.
-- advertiseSplit :: [(TopicName, a -> SomeMsg)] -> Topic IO a -> Node ()
-- advertiseSplit = undefined

-- |Get an action that will shutdown this Node.
getShutdownAction :: Node (IO ())
getShutdownAction = get >>= liftIO . readMVar . signalShutdown

-- |Apply any matching renames to a given name.
remapName :: String -> Node String
remapName name = asks (maybe name id . lookup name . nodeRemaps)

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

-- |Get the value associated with the given parameter name. If the
-- parameter is not set, then 'Nothing' is returned; if the parameter
-- is set to @x@, then @Just x@ is returned.
getParamOpt :: (XmlRpcType a, FromParam a) => String -> Node (Maybe a)
getParamOpt var = do var' <- remapName =<< canonicalizeName var
                     params <- nodeParams <$> ask
                     case lookup var' params of
                       Just val -> return . Just $ fromParam val
                       Nothing -> getServerParam var'

-- |Get the value associated with the given parameter name. If the
-- parameter is not set, return the second argument as the default
-- value.
getParam :: (XmlRpcType a, FromParam a) => String -> a -> Node a
getParam var def = maybe def id <$> getParamOpt var

-- |Get the current node's name.
getName :: Node String
getName = nodeName <$> get

-- |Get the current namespace.
getNamespace :: Node String
getNamespace = namespace <$> get

-- |Run a ROS Node.
runNode :: NodeName -> Node a -> IO ()
runNode name (Node nConf) = 
    do myURI <- newEmptyMVar
       sigStop <- newEmptyMVar
       env <- liftIO getEnvironment
       (conf, args) <- parseAppConfig <$> liftIO getArgs
       let getConfig' var def = maybe def id $ lookup var env
           getConfig = flip lookup env
           masterConf = getConfig' "ROS_MASTER_URI" "http://localhost:11311"
           namespaceConf = let ns = getConfig' "ROS_NAMESPACE" "/"
                           in if last ns == '/' then ns else ns ++ "/"
           (nameMap, params) = parseRemappings args
           name' = case lookup "__name" params of
                     Just x -> fromParam x
                     Nothing -> case name of
                                  '/':_ -> name
                                  _ -> namespaceConf ++ name
           -- Name remappings apply to exact strings and resolved names.
           resolve p@(('/':_),_) = [p]
           resolve (('_':n),v) = [(name'++"/"++n, v)]
           resolve (('~':n),v) = [(name'++"/"++ n, v)] --, ('_':n,v)]
           resolve (n,v) = [(namespaceConf ++ n,v), (n,v)]
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
       let configuredNode = runReaderT nConf (NodeConfig params' nameMap' conf)
           initialState = NodeState name' namespaceConf masterConf myURI
                                    sigStop M.empty M.empty
           statefulNode = execStateT configuredNode initialState
       statefulNode >>= flip runReaderT conf . RN.runNode name'
