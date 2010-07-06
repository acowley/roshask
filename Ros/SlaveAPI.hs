{-# LANGUAGE PackageImports #-}
module Ros.SlaveAPI (RosSlave(..), runSlave) where
import Control.Applicative
import Control.Concurrent (killThread, forkIO, threadDelay)
import Control.Concurrent.QSem
import "monads-fd" Control.Monad.Trans (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as BL
import Snap.Http.Server (httpServe)
import Snap.Types (Snap, getRequestBody, writeBS)
import Network.XmlRpc.Internals (Value, toValue)
import Network.XmlRpc.Server (handleCall, methods, fun)
import System.IO (hGetContents, hPutStr, hClose)
import System.Posix.Process (getProcessID)
import Ros.XmlRpcTuples
import Ros.RosTypes

class RosSlave a where
    getMaster :: a -> URI
    getSubscriptions :: a -> IO [(TopicName, TopicType, [(URI, SubStats)])]
    getPublications :: a -> IO [(TopicName, TopicType, [(URI, PubStats)])]
    publisherUpdate :: a -> TopicName -> [URI] -> IO ()
    getTopicPortTCP :: a -> TopicName -> Maybe Int
    stopNode :: a -> IO ()

type MessageData = String
type RpcResult a = IO (Int, String, a)

mkPublishStats :: (TopicName, a, [(URI, PubStats)]) -> 
                  (TopicName, MessageData, [(Int, Int, Int, Bool)])
mkPublishStats (n, _, pstats) = (n, "", map formatStats pstats)
    where formatStats (_, (PubStats bytesSent numSent conn)) = 
              (0, bytesSent, numSent, conn)

mkSubStats :: (TopicName, a, [(URI, SubStats)]) -> 
              (String, [(Int, Int, Int, Bool)])
mkSubStats (n, _, sstats) = (n, map formatStats sstats)
    where formatStats (_, (SubStats bytesReceived _ conn)) = 
              (0, bytesReceived, -1, conn)

getBusStats :: (RosSlave a) => a -> CallerID -> RpcResult [[Value]]
getBusStats n callerId = do
    publishStats <- map (toValue . mkPublishStats) <$> getPublications n
    subscribeStats <- map (toValue . mkSubStats) <$> getSubscriptions n
    let serviceStats = []
    return (1, "", [publishStats, subscribeStats, serviceStats])

getBusInfo :: (RosSlave a) => a -> CallerID -> RpcResult [[Value]]
getBusInfo n _ = do
    pubs <- map formatPubs <$> getPublications n
    subs <- map formatSubs <$> getSubscriptions n
    return (1, "", map (map toValue) (pubs ++ subs))
    where formatPubs (n, _, stats) = 
              map (\(u,_) -> (0::Int, u, "o", "TCPROS", n)) stats
          formatSubs (n, _, stats) = 
              map (\(u,_) -> (0::Int, u, "i", "TCPROS", n)) stats

getMaster' :: RosSlave a => a -> CallerID -> IO (Int, String, URI)
getMaster' n _ = return (1, "", getMaster n)

shutdown' :: RosSlave a => a -> QSem -> CallerID -> IO (Int, String, Bool)
shutdown' n q _ = stopNode n >> signalQSem q >> return (1, "", True)

getPid' :: RpcResult Int
getPid' = do pid <- getProcessID
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

pubUpdate :: RosSlave a => a -> CallerID -> TopicName -> [URI] -> RpcResult Bool
pubUpdate n _ topic publishers = do publisherUpdate n topic publishers
                                    return (1, "", True)

requestTopic :: RosSlave a => a -> CallerID -> TopicName -> [[Value]] -> 
                RpcResult Value
requestTopic n _ topic protocols = 
    case getTopicPortTCP n topic of
      Just p -> return (1, "", toValue ("TCPROS", p))
      Nothing -> return (0, "Unknown topic", toValue ("TCPROS", 0::Int))

-- Dispatch an XML-RPC request body and return the response. The first
-- parameter is a value that provides the necessary reflective API as
-- to ROS Node state. The second parameter is a semaphore indicating
-- that the node should terminate.
slaveRPC :: (RosSlave a) => a -> QSem -> String -> IO String
slaveRPC n = handleCall . dispatch
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

-- |Run a ROS slave node until it receives a shutdown command.
runSlave :: RosSlave a => a -> IO ()
runSlave n = do quitNow <- newQSem 0
                t <- forkIO $ simpleServe 9131 (rpc (slaveRPC n quitNow))
                waitQSem quitNow
                threadDelay 1000000 -- Wait a second for the response to flush
                stopNode n
                killThread t
    where rpc f = do body <- BLU.toString <$> getRequestBody
                     response <- liftIO $ f body
                     writeBS $ BU.fromString response
