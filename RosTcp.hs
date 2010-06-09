module RosTcp where
import Data.Binary (Binary)
import Data.ByteString
import Network
import ROSTypes

connect :: Binary a => URI -> IO (Stream a)
connect target = let (host,port) = parseURI target
                 in do h <- connectTo host port 
                       