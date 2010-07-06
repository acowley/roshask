module Ros.RunNode where
import Ros.MasterAPI
import Ros.SlaveAPI

myIP :: IO String
myIP = return "localhost"

-- What is the URI we're registering? Is it the same for all published topics?
-- We need to know the port of the XML-RPC server. 
registerPublication :: RosSlave n => n -> (TopicName, TopicType, a) -> IO ()
registerPublication n (tname, ttype, _) = 
    do uri <- registerPublisher master "roskell" tname ttype uri
    where Just port = getTopicPortTCP n tname
          uri = myIP ++ show port

registerNode :: RosSlave s => s -> Int -> IO ()
registerNode n port = 
    mapM_ (registerPublication n) (getPublications n) >>
    mapM_ (registerSubscription n) (getSubscriptions n)

runNode :: RosSlave s => s -> IO ()
runNode s = do (wait, port) <- runSlave s
               registerNode s port 
               wait