{-# LANGUAGE PackageImports #-}
module Ros.SlaveAPI (RosSlave(..), runSlave, requestTopicClient, 
                     cleanupNode) where
import Control.Applicative
import Control.Concurrent (killThread, forkIO, threadDelay, 
                           MVar, putMVar, readMVar)
import Control.Concurrent.QSem
import "monads-fd" Control.Monad.Trans (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as BL
import Snap.Http.Server (httpServe)
import Snap.Types (Snap, getRequestBody, writeBS)
import Network.Socket hiding (Stream)
import qualified Network.Socket as Net
import Network.XmlRpc.Internals (Value, toValue)
import Network.XmlRpc.Server (handleCall, methods, fun)
import Network.XmlRpc.Client (remote)
import System.IO (hGetContents, hPutStr, hClose)
import System.Posix.Process (getProcessID)
import System.Process (readProcess)
import Ros.Util.XmlRpcTuples
import Ros.RosTypes
import Ros.TopicStats
import Ros.MasterAPI

class RosSlave a where
    getMaster :: a -> URI
    getNodeName :: a -> String
    getNodeURI :: a -> MVar URI
    getSubscriptions :: a -> IO [(TopicName, TopicType, [(URI, SubStats)])]
    getPublications :: a -> IO [(TopicName, TopicType, [(URI, PubStats)])]
    publisherUpdate :: a -> TopicName -> [URI] -> IO ()
    getTopicPortTCP :: a -> TopicName -> Maybe Int
    stopNode :: a -> IO ()

-- |Unregister all of a node's publishers and subscribers, then stop
-- the node's servers.
cleanupNode :: RosSlave n => n -> IO ()
cleanupNode n = do pubs <- getPublications n
                   subs <- getSubscriptions n
                   nuri <- readMVar (getNodeURI n)
                   let nname = getNodeName n
                       master = getMaster n
                       stop f (tname,_,_) = f nname tname nuri
                   mapM_ (stop (unregisterPublisher master)) pubs
                   mapM_ (stop (unregisterSubscriber master)) subs
                   stopNode n

type MessageData = String
type RpcResult a = IO (Int, String, a)

mkPublishStats :: (TopicName, a, [(URI, PubStats)]) -> 
                  (TopicName, Int, [(Int, Int, Int, Bool)])
mkPublishStats (n, _, pstats) = (n, 0, map formatStats pstats)
    where formatStats (_, (PubStats bytesSent numSent conn)) = 
              (0, bytesSent, numSent, conn)

mkSubStats :: (TopicName, a, [(URI, SubStats)]) -> 
              (String, Int, [(Int, Int, Int, Bool)])
mkSubStats (n, _, sstats) = (n, 0, map formatStats sstats)
    where formatStats (_, (SubStats bytesReceived conn)) = 
              (0, bytesReceived, -1, conn)

getBusStats :: (RosSlave a) => a -> CallerID -> 
               RpcResult ([(String,Int,[(Int,Int,Int,Bool)])],
                          [(String,Int,[(Int,Int,Int,Bool)])],
                          (Int,Int,Int))
getBusStats n callerId = do
    publishStats <- map (mkPublishStats) <$> getPublications n
    subscribeStats <- map (mkSubStats) <$> getSubscriptions n
    let serviceStats = (0,0,0)
    return (1, "", (publishStats, subscribeStats, serviceStats))

getBusInfo :: (RosSlave a) => a -> CallerID -> 
              RpcResult [(Int,String,String,String,String)]
getBusInfo n _ = do
    pubs <- concatMap formatPubs <$> getPublications n
    subs <- concatMap formatSubs <$> getSubscriptions n
    return (1, "", pubs ++ subs)
    where formatPubs (tname, _, stats) = 
              map (\(uri,_) -> (0, uri, "o", "TCPROS", tname)) stats
          formatSubs (tname, _, stats) = 
              map (\(uri,_) -> (0, uri, "i", "TCPROS", tname)) stats

getMaster' :: RosSlave a => a -> CallerID -> IO (Int, String, URI)
getMaster' n _ = return (1, "", getMaster n)

shutdown' :: RosSlave a => a -> QSem -> CallerID -> IO (Int, String, Bool)
shutdown' n q _ = stopNode n >> signalQSem q >> return (1, "", True)

getPid' :: CallerID -> RpcResult Int
getPid' _ = do pid <- getProcessID
               return (1, "", fromEnum pid)

getSubscriptions' :: RosSlave a => a -> CallerID -> RpcResult [(String, String)]
getSubscriptions' n _ = do 
  subs <- map (\(n,t,_) -> (n,t)) <$> getSubscriptions n
  return (1, "", subs)

getPublications' :: RosSlave a => a -> CallerID -> RpcResult [(String, String)]
getPublications' n _ = do 
  pubs <- map (\(n,t,_) -> (n,t)) <$> getPublications n
  return (1, "", pubs)

paramUpdate' :: RosSlave a => a -> CallerID -> String -> Value -> RpcResult Bool
paramUpdate' n _ paramKey paramVal = do putStrLn "paramUpdate not implemented!"
                                        return (1, "", True)

pubUpdate :: RosSlave a => a -> CallerID -> TopicName -> [URI] -> RpcResult Int
pubUpdate n _ topic publishers = do publisherUpdate n topic publishers
                                    return (1, "", 1)

myName :: IO String
myName = init <$> readProcess "hostname" [] ""

requestTopic :: RosSlave a => a -> CallerID -> TopicName -> [[Value]] -> 
                RpcResult (String,String,Int)
requestTopic n _ topic protocols = 
    case getTopicPortTCP n topic of
      Just p -> do putStrLn $ topic++" requested "++show p
                   host <- myName
                   return (1, "", ("TCPROS",host,p))
      Nothing -> return (0, "Unknown topic", ("TCPROS", "", 0))

requestTopicClient :: URI -> CallerID -> TopicName -> [[String]] -> 
                      RpcResult (String,String,Int)
requestTopicClient = flip remote "requestTopic"

-- Dispatch an XML-RPC request body and return the response. The first
-- parameter is a value that provides the necessary reflective API as
-- to ROS Node state. The second parameter is a semaphore indicating
-- that the node should terminate.
slaveRPC :: (RosSlave a) => a -> QSem -> String -> IO String
slaveRPC n = -- \q s -> putStrLn ("Slave call "++s)>>(handleCall (dispatch q) s)
    handleCall . dispatch
    where dispatch q = methods [ ("getBusStats", fun (getBusStats n))
                               , ("getBusInfo", fun (getBusInfo n))
                               , ("getMasterUri", fun (getMaster' n))
                               , ("shutdown", fun (shutdown' n q))
                               , ("getPid", fun getPid')
                               , ("getSubscriptions", fun (getSubscriptions' n))
                               , ("getPublications", fun (getPublications' n))
                               , ("paramUpdate", fun (paramUpdate' n))
                               , ("publisherUpdate", fun (pubUpdate n))
                               , ("requestTopic", fun (requestTopic n)) ]

-- Start a Snap webserver on the specified port with the specified
-- handler.
simpleServe :: Int -> Snap () -> IO ()
simpleServe port handler = httpServe (pack "*") port (pack "myserver")
                                     Nothing Nothing handler
    where pack = BU.fromString

-- Find a free port by opening a socket, getting its port, then
-- closing it.
findFreePort :: IO Int
findFreePort = do s <- socket AF_INET Net.Stream defaultProtocol
                  bindSocket s (SockAddrInet aNY_PORT iNADDR_ANY)
                  port <- fromInteger . toInteger <$> socketPort s
                  sClose s
                  return port

-- |Run a ROS slave node. Returns an action that will wait for the
-- node to shutdown along with the port the server is running on.
runSlave :: RosSlave a => a -> IO (IO (), Int)
runSlave n = do quitNow <- newQSem 0
                port <- findFreePort
                myIP <- init <$> readProcess "hostname" [] ""
                putMVar (getNodeURI n) $ "http://"++myIP++":"++show port
                t <- forkIO $ simpleServe port (rpc (slaveRPC n quitNow))
                let wait = do waitQSem quitNow
                              -- Wait a second for the response to flush
                              threadDelay 1000000 
                              stopNode n
                              killThread t
                return (wait, port)
    where rpc f = do body <- BLU.toString <$> getRequestBody
                     response <- liftIO $ f body
                     writeBS $ BU.fromString response
