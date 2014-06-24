{-# LANGUAGE CPP, OverloadedStrings #-}
module Ros.Graph.Slave (RosSlave(..), runSlave, requestTopicClient,
                       cleanupNode) where
import Control.Applicative
import Control.Concurrent (killThread, forkIO, threadDelay, MVar, putMVar,
                           isEmptyMVar, readMVar, modifyMVar_)
import Control.Concurrent.SSem (SSem)
import qualified Control.Concurrent.SSem as Sem
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.UTF8 ()
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Snap.Http.Server (simpleHttpServe)
import Snap.Http.Server.Config (defaultConfig, setPort, Config, ConfigLog(..),
                                setVerbose, setAccessLog, setErrorLog)
-- import Snap.Types (Snap, getRequestBody, writeLBS, 
--                    getResponse, putResponse, setContentLength)
import Snap.Core (Snap, readRequestBody, writeLBS, getResponse, putResponse, 
                  setContentLength)
import Network.Socket hiding (Stream)
import qualified Network.Socket as Net
import Network.XmlRpc.Internals (Value)
import Network.XmlRpc.Server (handleCall, methods, fun)
import Network.XmlRpc.Client (remote)
#ifndef mingw32_HOST_OS
import System.Posix.Process (getProcessID)
#endif
import System.Process (readProcess)
import Ros.Internal.RosTypes
import Ros.Topic.Stats (PubStats(PubStats), SubStats(SubStats))
import Ros.Graph.Master

class RosSlave a where
    getMaster :: a -> URI
    getNodeName :: a -> String
    getNodeURI :: a -> MVar URI
    getSubscriptions :: a -> IO [(TopicName, TopicType, [(URI, SubStats)])]
    getPublications :: a -> IO [(TopicName, TopicType, [(URI, PubStats)])]
    publisherUpdate :: a -> TopicName -> [URI] -> IO ()
    getTopicPortTCP :: a -> TopicName -> Maybe Int
    setShutdownAction :: a -> IO () -> IO ()
    stopNode :: a -> IO ()

#ifdef mingw32_HOST_OS
getProcessID :: IO Int
getProcessID = return 42
#endif

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
getBusStats n _ = do
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

shutdown' :: RosSlave a => a -> SSem -> CallerID -> IO (Int, String, Bool)
shutdown' n q _ = stopNode n >> Sem.signal q >> return (1, "", True)

-- This requires a dependency on the unix package and so is not cross
-- platform.
getPid' :: CallerID -> RpcResult Int
getPid' _ = do pid <- getProcessID
               return (1, "", fromEnum pid)

getSubscriptions' :: RosSlave a => a -> CallerID -> RpcResult [(String, String)]
getSubscriptions' node _ = do 
  subs <- map (\(n,t,_) -> (n,t)) <$> getSubscriptions node
  return (1, "", subs)

getPublications' :: RosSlave a => a -> CallerID -> RpcResult [(String, String)]
getPublications' node _ = do 
  pubs <- map (\(n,t,_) -> (n,t)) <$> getPublications node
  return (1, "", pubs)

paramUpdate' :: RosSlave a => a -> CallerID -> String -> Value -> RpcResult Bool
paramUpdate' _n _ _paramKey _paramVal = do 
  putStrLn "paramUpdate not implemented!"
  return (1, "", True)

pubUpdate :: RosSlave a => a -> CallerID -> TopicName -> [URI] -> RpcResult Int
pubUpdate n _ topic publishers = do publisherUpdate n topic publishers
                                    return (1, "", 1)

-- Extract just the hostname or IP part of a Node's URI.
myName :: RosSlave a => a -> IO String
myName n = extractName `fmap` readMVar (getNodeURI n)
    where extractName uri = takeWhile (/=':') $ drop 7 uri

requestTopic :: RosSlave a => a -> CallerID -> TopicName -> [[Value]] -> 
                RpcResult (String,String,Int)
requestTopic n _ topic _protocols = 
    case getTopicPortTCP n topic of
      Just p -> do --putStrLn $ topic++" requested "++show p
                   host <- myName n
                   return (1, "", ("TCPROS",host,p))
      Nothing -> return (0, "Unknown topic", ("TCPROS", "", 0))

requestTopicClient :: URI -> CallerID -> TopicName -> [[String]] -> 
                      RpcResult (String,String,Int)
requestTopicClient = flip remote "requestTopic"

-- Dispatch an XML-RPC request body and return the response. The first
-- parameter is a value that provides the necessary reflective API as
-- to ROS Node state. The second parameter is a semaphore indicating
-- that the node should terminate.
slaveRPC :: (RosSlave a) => a -> SSem -> String -> IO BLU.ByteString
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
simpleServe port handler = simpleHttpServe conf handler
  where conf :: Config Snap ()
        conf = setAccessLog ConfigNoLog .
               setErrorLog ConfigNoLog . 
               setVerbose False .
               setPort port $
               defaultConfig

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
runSlave n = do quitNow <- Sem.new 0
                port <- findFreePort
                let myUri = getNodeURI n
                    myPort = ":" ++ show port
                myURIEmpty <- isEmptyMVar myUri
                if myURIEmpty 
                  then do myIP <- init <$> readProcess "hostname" [] ""
                          putMVar myUri $! "http://"++myIP++myPort
                  else modifyMVar_ myUri ((return $!) . (++myPort))
                t <- forkIO $ simpleServe port (rpc (slaveRPC n quitNow))
                let wait = do Sem.wait quitNow
                              -- Wait a second for the response to flush
                              threadDelay 1000000 
                              stopNode n
                              killThread t
                return (wait, port)
    where rpc f = do body <- BLU.toString <$> readRequestBody 4096
                     response <- liftIO $ f body
                     writeLBS response
                     let len = fromIntegral $ BLU.length response
                     putResponse . setContentLength len =<< getResponse
