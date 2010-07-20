module Ros.RunNode where
import Ros.RosTypes
import Ros.MasterAPI
import Ros.SlaveAPI

-- FIXME: We should check /etc/hosts for our hostname to see if
-- there's another IP address listed there.
myIP :: IO String
myIP = return "127.0.0.1"

-- Inform the master that we are publishing a particular topic.
registerPublication :: RosSlave n => 
                       n -> String -> String -> (TopicName, TopicType, a) -> IO ()
registerPublication n master uri (tname, ttype, _) = 
    do subscribers <- registerPublisher master "roskell" tname ttype uri
       return ()

-- Inform the master that we are subscribing to a particular topic.
registerSubscription :: RosSlave n =>
                        n -> String -> String -> (TopicName, TopicType, a) -> IO ()
registerSubscription n master uri (tname, ttype, _) = 
    do publishers <- registerSubscriber master "roskell" tname ttype uri
       return ()

registerNode :: RosSlave s => s -> Int -> IO ()
registerNode n port = 
    do ip <- myIP
       let uri = "http://"++ip++":"++show port
           master = getMaster n
       putStrLn $ "Starting roshask node at " ++ uri
       getPublications n >>= mapM_ (registerPublication n master uri)
       getSubscriptions n >>= mapM_ (registerSubscription n master uri)

runNode :: RosSlave s => s -> IO ()
runNode s = do (wait, port) <- runSlave s
               registerNode s port 
               wait