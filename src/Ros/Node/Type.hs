{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs,
             ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Ros.Node.Type where
import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent (MVar, putMVar)
import Control.Concurrent.STM (atomically, TVar, readTVar, writeTVar)
import Control.Monad.State
import Control.Monad.Reader
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Concurrent (ThreadId)
import Ros.Internal.RosTypes (URI)
import Ros.Internal.Util.ArgRemapping (ParamVal)
import Ros.Internal.Util.AppConfig (ConfigOptions)
import Ros.Graph.Slave (RosSlave(..))
import Ros.Topic (Topic)
import Ros.Topic.Stats

data Subscription = Subscription { knownPubs :: TVar (Set URI)
                                 , addPub    :: URI -> IO ThreadId
                                 , subType   :: String
                                 , subStats  :: StatMap SubStats }

data DynTopic where
  DynTopic :: Typeable a => Topic IO a -> DynTopic

fromDynTopic :: Typeable a => DynTopic -> Maybe (Topic IO a)
fromDynTopic (DynTopic t) = gcast t

data Publication = Publication { subscribers :: TVar (Set URI)
                               , pubType     :: String
                               , pubPort     :: Int
                               , pubCleanup  :: IO ()
                               , pubTopic    :: DynTopic
                               , pubStats    :: StatMap PubStats }

data NodeState = NodeState { nodeName       :: String
                           , namespace      :: String
                           , master         :: URI
                           , nodeURI        :: MVar URI
                           , signalShutdown :: MVar (IO ())
                           , subscriptions  :: Map String Subscription
                           , publications   :: Map String Publication }

type Params = [(String, ParamVal)]
type Remap = [(String,String)]

data NodeConfig = NodeConfig { nodeParams :: Params
                             , nodeRemaps :: Remap
                             , nodeAppConfig :: ConfigOptions }

-- |A 'Node' carries with it parameters, topic remappings, and some
-- state encoding the status of its subscriptions and publications.
newtype Node a = Node { unNode :: ReaderT NodeConfig (StateT NodeState IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadState NodeState Node where
    get = Node get
    put = Node . put

instance MonadReader NodeConfig Node where
    ask = Node ask
    local f m = Node $ withReaderT f (unNode m)

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
                                   (act',known') <- foldM (connectToPub add)
                                                          (return (), known)
                                                          uris
                                   writeTVar (knownPubs sub) known'
                                   return act'
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
