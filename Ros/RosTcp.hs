module Ros.RosTcp (subStream, runServer) where
import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.BoundedChan
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Monad (forever, forM_)
import Data.Binary.Put (runPut)
import Data.ByteString.Lazy (ByteString)
import Network.BSD (getHostByName, hostAddress)
import Network.Socket hiding (send, sendTo, recv, recvFrom, Stream)
import qualified Network.Socket as Sock
import Network.Socket.ByteString.Lazy
import System.IO (IOMode(ReadMode))

import Ros.BinaryIter
import Ros.RosTypes
import Ros.RosBinary

-- |Maximum number of items to buffer for each client.
sendBufferSize :: Int
sendBufferSize = 10

-- |Push each item from this client's buffer over the connected
-- socket.
serviceClient :: BoundedChan ByteString -> Socket -> IO ()
serviceClient c s = go
    where go = readChan c >>= sendAll s >> go

-- |Accept new client connections. A new send buffer is allocated for
-- each new client and added to the client list along with an action
-- for cleaning up the client connection.
acceptClients :: Socket -> TVar [(IO (), BoundedChan ByteString)] -> IO ()
acceptClients sock clients = forever acceptClient
    where acceptClient = do (client,_) <- accept sock
                            chan <- newBoundedChan sendBufferSize
                            t <- forkIO $ serviceClient chan client
                            let cleanup = shutdown client ShutdownBoth >>
                                          killThread t
                            atomically $ readTVar clients >>= 
                                         writeTVar clients . ((cleanup,chan) :)

-- |Publish each item obtained from a Stream to each connected client.
pubStream :: BinaryCompact a => 
             Stream a -> TVar [(b, BoundedChan ByteString)] -> IO ()
pubStream s clients = forever $ go s
    where go (Stream x xs) = let bytes = runPut (put x)
                             in atomically (readTVar clients) >>=
                                mapM_ (flip writeChan bytes . snd)

-- |Connect to a publisher and return the stream of data it is
-- publishing.
subStream :: BinaryIter a => URI -> IO (Stream a)
subStream target = do sock <- socket AF_INET Sock.Stream defaultProtocol
                      ip <- hostAddress <$> getHostByName host
                      connect sock $ SockAddrInet port ip
                      h <- socketToHandle sock ReadMode
                      streamIn h
    where (host, port) = parseLocation target
          parseLocation = (id *** fromIntegral.read) . break (== ':')

-- |The server starts a thread that peels elements off the stream as
-- they become available and sends them to all connected
-- clients. Returns an action for cleanup up resources allocated by
-- this publication server along with the port the server is listening
-- on.
runServer :: BinaryCompact a => Stream a -> IO (IO (), Int)
runServer stream = withSocketsDo $ do
                     sock <- socket AF_INET Sock.Stream defaultProtocol
                     bindSocket sock (SockAddrInet aNY_PORT iNADDR_ANY)
                     port <- fromIntegral <$> socketPort sock
                     listen sock 5
                     clients <- newTVarIO []
                     acceptThread <- forkIO $ acceptClients sock clients
                     pubThread <- forkIO $ pubStream stream clients
                     let cleanup = atomically (readTVar clients) >>= 
                                   sequence_ . map fst >> 
                                   shutdown sock ShutdownBoth >>
                                   killThread acceptThread
                     return (cleanup, port)
