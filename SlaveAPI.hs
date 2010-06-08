{-# LANGUAGE PackageImports #-}
module SlaveAPI (RosSlave(..), runSlave) where
import Control.Applicative
import Control.Arrow (second)
import Control.Concurrent (killThread, forkIO)
import Control.Concurrent.QSem
import Control.Monad
import "mtl" Control.Monad.Error (throwError)
import "monads-fd" Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Snap.Http.Server
import Snap.Types
import Network.Socket (recv)
import Network.XmlRpc.Internals
import Network.XmlRpc.Server
import System.IO (hGetContents, hPutStr, hClose)
import System.Posix.Process (getProcessID)
import XmlRpcTuples
import ROSTypes

class RosSlave a where
    getMaster :: a -> URI
    getSubscriptions :: a -> [(TopicName, TopicType, [SubStats])]
    getPublications :: a -> [(TopicName, TopicType, [PubStats])]
    publisherUpdate :: a -> TopicName -> [URI] -> IO ()
    getTopicPortTCP :: a -> TopicName -> Int
    stopNode :: a -> IO ()

type MessageData = String
type PublishStatsX = (TopicName, MessageData, [(Int, Int, Int, Bool)])
type RpcResult a = IO (Int, String, a)

mkPublishStats :: (TopicName, a, [PubStats]) -> PublishStatsX
mkPublishStats (n, _, pstats) = (n, "", map formatStats pstats)
    where formatStats (PubStats bytesSent numSent _ conn) = 
              (0, bytesSent, numSent, conn)

mkSubStats :: (TopicName, a, [SubStats]) -> (String, [(Int, Int, Int, Bool)])
mkSubStats (n, _, sstats) = (n, map formatStats sstats)
    where formatStats (SubStats bytesReceived _ conn) = 
              (0, bytesReceived, -1, conn)

getBusStats :: (RosSlave a) => a -> CallerID -> RpcResult [[Value]]
getBusStats n callerId = 
    return (1, "", [publishStats, subscribeStats, serviceStats])
    where serviceStats = []
          publishStats = map (toValue . mkPublishStats) (getPublications n)
          subscribeStats = map (toValue . mkSubStats) (getSubscriptions n)

getBusInfo :: (RosSlave a) => a -> CallerID -> RpcResult [[Value]]
getBusInfo n _ = 
    return (1, "", map (map toValue) (pubs ++ subs))
    where pubs = map formatPubs (getPublications n)
          subs = map formatSubs (getSubscriptions n)
          formatPubs (n, _, stats) = 
              map (\c -> (0::Int, pubDestination c, "o", "TCPROS", n)) stats
          formatSubs (n, _, stats) = 
              map (\c -> (0::Int, subDestination c, "i", "TCPROS", n)) stats

getMaster' :: RosSlave a => a -> CallerID -> IO (Int, String, URI)
getMaster' n _ = return (1, "", getMaster n)

shutdown' :: RosSlave a => a -> QSem -> CallerID -> IO (Int, String, Bool)
shutdown' n q _ = stopNode n >> signalQSem q >> return (1, "", True)

getPid' :: RpcResult Int
getPid' = do pid <- getProcessID
             return (1, "", fromEnum pid)

getSubscriptions' :: RosSlave a => a -> CallerID -> RpcResult [(String, String)]
getSubscriptions' n _ = return (1, "", subs)
    where subs = map (\(n,t,_) -> (n,t)) (getSubscriptions n)

getPublications' :: RosSlave a => a -> CallerID -> RpcResult [(String, String)]
getPublications' n _ = return (1, "", pubs)
    where pubs = map (\(n,t,_) -> (n,t)) (getPublications n)

paramUpdate' :: RosSlave a => a -> CallerID -> String -> Value -> RpcResult Bool
paramUpdate' n _ paramKey paramVal = do putStrLn "paramUpdate not implemented!"
                                        return (1, "", True)

pubUpdate :: RosSlave a => a -> CallerID -> TopicName -> [URI] -> RpcResult Bool
pubUpdate n _ topic publishers = do publisherUpdate n topic publishers
                                    return (1, "", True)

requestTopic :: RosSlave a => a -> CallerID -> TopicName -> [[Value]] -> 
                RpcResult Value
requestTopic n _ topic protocols = return (1, "", toValue protocolInfo)
    where protocolInfo = ("TCPROS", getTopicPortTCP n topic)

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

simpleServe :: Int -> Snap () -> IO ()
simpleServe port handler = httpServe (pack "*") port (pack "myserver")
                                     Nothing Nothing handler
    where pack = B.pack . map (toEnum . fromEnum)

runSlave :: RosSlave a => a -> IO ()
runSlave n = do quitNow <- newQSem 0
                t <- forkIO $ simpleServe 9131 (rpc (slaveRPC n quitNow))
                waitQSem quitNow
                killThread t
    where rpc f = do body <- BL.foldr ((:) . toEnum . fromEnum) [] <$> 
                             getRequestBody
                     response <- liftIO $ f body
                     writeBS $ B.pack (map (toEnum . fromEnum) response)
