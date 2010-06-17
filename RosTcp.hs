module RosTcp where
import Data.Binary (Binary)
import Data.ByteString
import Network
import ROSTypes

connect :: Binary a => URI -> IO (Stream a)
connect target = let (host,port) = parseURI target
                 in do h <- connectTo host port 

-- The server starts a thread that peels elements off the stream as
-- they become available and sends them to any connected clients. Is
-- that right? We need to accept connections from new clients, but we
-- want to record statistics and status for each connection, which
-- means that we need some feedback from here to the caller. We also
-- probably want to setup a channel for each client. When new items
-- come from the stream, we push them into each channel, which is then
-- serviced by a separate thread for each connected client.
runServer :: Binary a => Stream a -> IO (ThreadId, Int)
runServer s = do sock <- listenOn (PortNumber (fromIntegral freePort))
                 let acceptClients = do accept sock 
                 t <- forkIO (acceptClients sock)
                 return (sock, t)
    where acceptClients 
                       