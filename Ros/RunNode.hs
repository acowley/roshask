module Ros.RunNode where
import Control.Concurrent (readMVar)
import Control.Concurrent.QSem (signalQSem)
import System.Posix.Signals (installHandler, Handler(..), sigINT)
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
    do putStrLn $ "Registering publication of "++ttype++" on topic "++
                  tname++" on master "++master
       subscribers <- registerPublisher master name tname ttype uri
       return ()

-- Inform the master that we are subscribing to a particular topic.
registerSubscription :: RosSlave n =>
                        String -> n -> String -> String -> 
                        (TopicName, TopicType, a) -> IO ()
registerSubscription name n master uri (tname, ttype, _) = 
    do putStrLn $ "Registring subscription to "++tname++" for "++ttype++"s"
       fr@(r,_,publishers) <- registerSubscriber master name tname ttype uri
       if r == 1 
         then publisherUpdate n tname publishers
         else error "Failed to register subscriber with master"
       return ()

registerNode :: RosSlave s => String -> s -> Int -> IO ()
registerNode name n port = 
    do uri <- readMVar (getNodeURI n)
       let master = getMaster n
       putStrLn $ "Starting node "++name++" at " ++ uri
       getPublications n >>= mapM_ (registerPublication name n master uri)
       getSubscriptions n >>= mapM_ (registerSubscription name n master uri)

runNode :: RosSlave s => String -> s -> IO ()
runNode name s = do (wait, port) <- runSlave s
                    registerNode name s port 
                    putStrLn "Spinning"
                    let shutdown = putStrLn "Shutting down" >> 
                                   cleanupNode s
                                   
                    installHandler sigINT (CatchOnce shutdown) Nothing
                    wait