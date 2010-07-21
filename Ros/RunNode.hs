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
                       String -> n -> String -> String -> 
                       (TopicName, TopicType, a) -> IO ()
registerPublication name n master uri (tname, ttype, _) = 
    do let ttype' = "std_msgs/String"
       putStrLn $ "Registering publication on master "++master++
                  " of "++ttype'++" on "++tname
       subscribers <- registerPublisher master name tname ttype' uri
       return ()

-- Inform the master that we are subscribing to a particular topic.
registerSubscription :: RosSlave n =>
                        String -> n -> String -> String -> 
                        (TopicName, TopicType, a) -> IO ()
registerSubscription name n master uri (tname, ttype, _) = 
    do publishers <- registerSubscriber master name tname ttype uri
       return ()

registerNode :: RosSlave s => String -> s -> Int -> IO ()
registerNode name n port = 
    do ip <- myIP
       let uri = "http://"++ip++":"++show port
           master = getMaster n
       putStrLn $ "Starting roshask node at " ++ uri
       getPublications n >>= mapM_ (registerPublication name n master uri)
       getSubscriptions n >>= mapM_ (registerSubscription name n master uri)

runNode :: RosSlave s => String -> s -> IO ()
runNode name s = do putStrLn "Starting XML-RPC Server"
                    (wait, port) <- runSlave s
                    putStrLn $ "Registering Node "++name
                    registerNode name s port 
                    putStrLn "Spinning"
                    wait