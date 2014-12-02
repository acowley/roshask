{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Ros.Node.RosTcp (subStream, runServer, runServers, callServiceWithMaster) where
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import qualified Control.Exception as E
import Control.Monad.Reader
import Data.Binary.Put (runPut, putWord32le)
import Data.Binary.Get (runGet, getWord32le)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Network.BSD (getHostByName, hostAddress)
import Network.Socket hiding (send, sendTo, recv, recvFrom, Stream, ServiceName)
import qualified Network.Socket as Sock
import Network.Socket.ByteString
import Prelude hiding (getContents)

import System.IO (IOMode(ReadMode), hClose)
import Text.URI (parseURI, uriRegName, uriPort)

import Ros.Node.BinaryIter (streamIn, getServiceResult)
import Ros.Internal.Msg.MsgInfo
import Ros.Internal.Msg.SrvInfo
import Ros.Internal.RosBinary
import Ros.Internal.RosTypes
import Ros.Internal.Util.RingChan
import Ros.Internal.Util.AppConfig (Config, debug, forkConfig)
import Ros.Topic (Topic(..))
import Ros.Node.ConnectionHeader
import Ros.Graph.Slave (requestTopicClient)
import Ros.Graph.Master (lookupService)
import Data.Maybe (fromMaybe)
import Ros.Service.ServiceTypes
import Control.Monad.Except
import System.IO.Error (tryIOError)

-- |Push each item from this client's buffer over the connected
-- socket.
serviceClient :: RingChan ByteString -> Socket -> IO ()
serviceClient c s = forever $ do bs <- readChan c
                                 sendBS s bs

sendBS :: Socket -> ByteString -> IO ()
sendBS sock bs =
  let len = runPut $ 
            putWord32le . fromIntegral $ 
            BL.length bs
  in
   sendAll sock (BL.toStrict $ BL.append len bs)

recvAll :: Socket -> Int -> IO B.ByteString
recvAll s = flip go []
    where go len acc = do bs <- recv s len
                          if B.length bs < len
                            then go (len - B.length bs) (bs:acc)
                            else return $ B.concat (reverse (bs:acc))

negotiatePub :: String -> String -> Socket -> IO ()
negotiatePub ttype md5 sock = 
    do headerLength <- runGet (fromIntegral <$> getWord32le) <$>
                       BL.fromChunks . (:[]) <$> recvAll sock 4
       headerBytes <- recvAll sock headerLength
       let connHeader = parseHeader headerBytes
           wildCard = case lookup "type" connHeader of
                        Just t | t == "*" -> True
                               | t == ttype -> False
                               | otherwise -> error $ 
                                              "Disagreeing Topic types: " ++
                                              "publisher expected "++ttype++
                                              ", but client asked for "++t
                        Nothing -> error $ "Client did not include the "++
                                           "topic type in its "++
                                           "connection request."
       when (not wildCard) 
            (case lookup "md5sum" connHeader of
               Just s | s == md5 -> return ()
                      | otherwise -> error "Disagreement on Topic type MD5"
               Nothing -> error $ "Client did not include MD5 sum "++
                                  "in its request.")
       case lookup "tcp_nodelay" connHeader of
         Just "1" -> setSocketOption sock NoDelay 0
         _ -> return ()
       sendAll sock . genHeader $
         [("md5sum",md5), ("type",ttype), ("callerid","roshask")]

-- |Accept new client connections. A new send buffer is allocated for
-- each new client and added to the client list along with an action
-- for cleaning up the client connection.
-- FIXME: cleaning up a disconnected client should be reflected at a
-- higher level, too.
acceptClients :: Socket -> TVar [(Config (), RingChan ByteString)] -> 
                 (Socket -> IO ()) -> IO (RingChan ByteString) -> Config ()
acceptClients sock clients negotiate mkBuffer = forever acceptClient
    where acceptClient = do (client,_) <- liftIO $ accept sock
                            debug "Accepted client socket"
                            liftIO $ negotiate client
                            chan <- liftIO mkBuffer
                            let cleanup1 = 
                                    do debug "Closing client socket"
                                       liftIO $ 
                                         shutdown client ShutdownBoth `E.catch`
                                           \(_::E.SomeException) -> return ()
                            r <- ask
                            t <- liftIO . forkIO $ 
                                 serviceClient chan client `E.catch` 
                                   \(_::E.SomeException) -> runReaderT cleanup1 r
                            let cleanup2 = cleanup1 >>
                                           (liftIO $ killThread t)
                            liftIO . atomically $ 
                              readTVar clients >>= 
                              writeTVar clients . ((cleanup2,chan) :)

-- |Publish each item obtained from a 'Topic' to each connected client.
pubStream :: RosBinary a
          => Topic IO a -> TVar [(b, RingChan ByteString)] -> Config ()
pubStream t0 clients = liftIO $ go 0 t0
  where go !n t = do (x, t') <- runTopic t
                     let bytes = runPut $ putMsg n x
                     cs <- readTVarIO clients
                     mapM_ (flip writeChan bytes . snd) cs
                     go (n+1) t'

-- |Produce a publishing action associated with a list of
-- clients. This is used by runServers.
pubStreamIO :: RosBinary a => IO (TVar [(b, RingChan ByteString)] -> Config (), 
                                  a -> IO ())
pubStreamIO = do m <- newEmptyMVar
                 let feed clients = 
                       let go !n = do x <- takeMVar m
                                      let bytes = runPut $ putMsg n x
                                      cs <- readTVarIO clients
                                      mapM_ (flip writeChan bytes . snd) cs
                                      go (n+1)
                       in liftIO $ go 0
                 return (feed, putMVar m)

-- Negotiate a TCPROS subscriber connection.
-- Precondition: The socket is connected
negotiateSub :: Socket -> String -> String -> String -> IO ()
negotiateSub sock tname ttype md5 = 
    do sendAll sock $ genHeader [ ("callerid", "roshask"), ("topic", tname)
                                , ("md5sum", md5), ("type", ttype) 
                                , ("tcp_nodelay", "1") ]
       responseLength <- runGet (fromIntegral <$> getWord32le) <$>
                         BL.fromChunks . (:[]) <$> recvAll sock 4
       headerBytes <- recvAll sock responseLength
       let connHeader = parseHeader headerBytes
       case lookup "type" connHeader of
         Just t | t == ttype -> return ()
                | otherwise -> error $ "Disagreeing Topic types: " ++
                                       "subscriber expected "++ttype++
                                       ", but server replied with "++t
         Nothing -> error $ "Server did not include the topic type "++
                            "in its response."
       case lookup "md5sum" connHeader of
         Just s | s == md5 -> return ()
                | otherwise -> error "Disagreement on Topic type MD5"
         Nothing -> error "Server did not include MD5 sum in its response."
       setSocketOption sock KeepAlive 1

-- |Connect to a publisher and return the stream of data it is
-- publishing.
subStream :: forall a. (RosBinary a, MsgInfo a) => 
             URI -> String -> (Int -> IO ()) -> Config (Topic IO a)
subStream target tname _updateStats = 
    do debug $ "Opening stream to " ++target++" for "++tname
       h <- liftIO $ 
            do response <- requestTopicClient target "/roshask" tname 
                                              [["TCPROS"]]
               let port = case response of
                            (1,_,("TCPROS",_,port')) -> fromIntegral port'
                            _ -> error $ "Couldn't get publisher's port for "++
                                         tname++" from node "++target
               sock <- socket AF_INET Sock.Stream defaultProtocol
               ip <- hostAddress <$> getHostByName host
               connect sock $ SockAddrInet port ip
               let md5 = sourceMD5 (undefined::a)
                   ttype = msgTypeName (undefined::a)
               negotiateSub sock tname ttype md5
               socketToHandle sock ReadMode
       --hSetBuffering h NoBuffering
       debug $ "Streaming "++tname++" from "++target
       return $ streamIn h
    where host = parseHost target

parseHost :: URI -> String
parseHost target = case parseURI target of
  Just u -> fromMaybe
            (error $ "Couldn't parse hostname "++ "from "++target)
            (uriRegName u)
  Nothing -> error $ "Couldn't parse URI "++target

parseHostAndPort :: URI -> Either ServiceResponseExcept (String, PortNumber)
parseHostAndPort target = do
  uri <- maybeToEither (ConnectExcept $ "Could not parse URI "++target) $ parseURI target
  host <- maybeToEither (ConnectExcept $ "Could not parse hostname from "++target) $ uriRegName uri
  port <- maybeToEither (ConnectExcept $ "Could not parse port from "++target) $ uriPort uri
  return (host, fromIntegral port)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither left m = case m of
  Just x -> Right x
  Nothing -> Left left

--TODO: use the correct callerID
callServiceWithMaster :: forall a b. (RosBinary a, SrvInfo a, RosBinary b, SrvInfo b) =>
                         URI -> ServiceName -> a -> IO (Either ServiceResponseExcept b)
callServiceWithMaster rosMaster serviceName message = runExceptT $ do
  checkServicesMatch message (undefined::b)
  --lookup the service with the master
  (code, statusMessage, serviceUrl) <- lookupService rosMaster callerID serviceName
  checkLookupServiceCode code statusMessage
  (host, port) <- ExceptT . return $ parseHostAndPort serviceUrl
  -- make a socket
  let makeSocket = socket AF_INET Sock.Stream defaultProtocol
      closeSocket sock = liftIO $ sClose sock
      withSocket sock = do
        ioErrorToExceptT ConnectExcept "Problem connecting to server. Got exception : " $ do
          --Connect to the socket
          ip <- hostAddress <$> getHostByName host
          connect sock $ SockAddrInet port ip
        let reqMd5 = srvMD5 message
            reqServiceType = srvTypeName message
        negotiateService sock serviceName reqServiceType reqMd5
        let bytes = runPut $ putMsg 0 message
        ioErrorToExceptT SendRequestExcept "Problem sending request. Got exception: " $
          sendBS sock bytes
        liftIO $ socketToHandle sock ReadMode
  handle <- bracketOnErrorME (liftIO makeSocket) closeSocket withSocket
  result <- getServiceResult handle
  liftIO $ hClose handle
  return result
    where
      callerID = "roshask"
      checkLookupServiceCode 1 _ = return ()
      checkLookupServiceCode code statusMessage =
        throwError $ MasterExcept
        ("lookupService failed, code: " ++ show code ++ ", statusMessage: " ++ statusMessage)
      checkServicesMatch x y =
        unless match $
          -- throw error here since the calling code needs to be changed
          error "Request and response type do not match"
        where
          match = srvMD5 x == srvMD5 y && srvTypeName x == srvTypeName y

-- | bracketOnError equivalent for MonadError
bracketOnErrorME :: MonadError e m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnErrorME before after thing = do
  a <- before
  let handler e = after a >> throwError e
  catchError (thing a) handler

-- | Catch any IOErrors and convert them to a different type
catchConvertIO :: (String -> a) -> IO b -> IO (Either a b)
catchConvertIO excep action = do
  err <- tryIOError action
  return $ case err of
    Left e -> Left . excep $ show e
    Right r -> Right r

-- | Catch all IOErrors that might occur and convert them to a custom error type
-- with the IOError message postpended to the given string
ioErrorToExceptT :: (String -> e) -> String -> IO a -> ExceptT e IO a
ioErrorToExceptT except msg acc =
  ExceptT . catchConvertIO (\m -> except $ msg ++ m) $ acc

-- Precondition: The socket is already connected to the server
-- Exchange ROSTCP connection headers with the server
negotiateService :: Socket -> String -> String -> String -> ExceptT ServiceResponseExcept IO ()
negotiateService sock serviceName serviceType md5 = do
    headerBytes <- liftIO $
      do sendAll sock $ genHeader [ ("callerid", "roshask"), ("service", serviceName)
                                , ("md5sum", md5), ("type", serviceType) ]
         responseLength <- runGet (fromIntegral <$> getWord32le) <$>
                           BL.fromChunks . (:[]) <$> recvAll sock 4
         recvAll sock responseLength
    let connHeader = parseHeader headerBytes
    case lookup "error" connHeader of
      Nothing -> return ()
      Just _ -> throwError . ConHeadExcept $
                "Connection header from server has error, connection header is: " ++ show connHeader
                                    
-- Helper to run the publisher's side of a topic negotiation with a
-- new client.
mkPubNegotiator :: MsgInfo a => a -> Socket -> IO ()
mkPubNegotiator x = negotiatePub (msgTypeName x) (sourceMD5 x)

-- Run a publication server given a function that returns a
-- negotiation action given a client 'Socket', a function that returns
-- a publication action given a client list, a statistics updater, and
-- the size of the send buffer.
runServerAux :: (Socket -> IO ()) -> 
                (TVar [(Config (), RingChan ByteString)] -> Config ()) -> 
                (URI -> Int -> IO ()) -> Int -> Config (Config (), Int)
runServerAux negotiate pubAction _updateStats bufferSize = 
    do r <- ask
       liftIO . withSocketsDo $ runReaderT go r
  where go = do sock <- liftIO $ socket AF_INET Sock.Stream defaultProtocol
                liftIO $ bindSocket sock (SockAddrInet aNY_PORT iNADDR_ANY)
                port <- liftIO (fromInteger . toInteger <$> socketPort sock)
                liftIO $ listen sock 5
                clients <- liftIO $ newTVarIO []
                let mkBuffer = newRingChan bufferSize
                acceptThread <- forkConfig $
                                acceptClients sock clients negotiate mkBuffer
                pubThread <- forkConfig $ pubAction clients
                let cleanup = liftIO (atomically (readTVar clients)) >>= 
                              sequence_ . map fst >> 
                              liftIO (shutdown sock ShutdownBoth >>
                                      killThread acceptThread >>
                                      killThread pubThread)
                return (cleanup, port)

-- |The server starts a thread that peels elements off the stream as
-- they become available and sends them to all connected
-- clients. Returns an action for cleaning up resources allocated by
-- this publication server along with the port the server is listening
-- on.
runServer :: forall a. (RosBinary a, MsgInfo a) => 
             Topic IO a -> (URI -> Int -> IO ()) -> Int -> 
             Config (Config (), Int)
runServer stream = runServerAux (mkPubNegotiator (undefined::a)) 
                                (pubStream stream)

-- |The 'MsgInfo' type class dictionary made explicit to strip off the
-- actual message type.
data MsgInfoRcd = MsgInfoRcd { _md5, _typeName :: String }

-- |A 'Feeder' represents a 'Topic' fully prepared to accept
-- subscribers.
data Feeder = Feeder MsgInfoRcd -- Explicit MsgInfo dictionary
                     Int -- Transmit buffer size
                     (URI -> Int -> IO ()) -- Update topic stats
                     (TVar [(Config (), RingChan ByteString)] -> Config ())
                     -- 'pubStream' partial application

-- |Prepare an action for publishing messages. Arguments are a monadic
-- function for updating topic statistics, and a transmit buffer
-- size. The returned 'Feeder' value may be supplied to 'runServers',
-- while the returned 'IO' function may be used to push out new
-- messages.
feedTopic :: forall a. (MsgInfo a, RosBinary a) => 
             (URI -> Int -> IO ()) -> Int -> IO (Feeder, a -> IO ())
feedTopic updateStats bufSize = 
  do (feed,pub) <- pubStreamIO
     let f = Feeder info bufSize updateStats feed
     return (f, pub)
  where info = mkInfo (undefined::a)
        mkInfo x = MsgInfoRcd (msgTypeName x) (sourceMD5 x)

-- |Publish several 'Topic's. A single cleanup action for all 'Topic's
-- is returned, along with each 'Topic's server port in the order of
-- the input 'Feeder's.
runServers :: [Feeder] -> Config (Config (), [Int])
runServers = return . first sequence_ . unzip <=< mapM feed
  where feed (Feeder (MsgInfoRcd md5 typeName) bufSize stats push) = 
          let pub = negotiatePub typeName md5
          in runServerAux pub push stats bufSize
