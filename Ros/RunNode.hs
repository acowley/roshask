module Ros.RunNode (runNode) where
import Control.Concurrent (readMVar,forkIO, killThread)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import System.Posix.Signals (installHandler, Handler(..), sigINT)
import Ros.RosTypes
import Ros.MasterAPI
import Ros.SlaveAPI

-- Inform the master that we are publishing a particular topic.
registerPublication :: RosSlave n => 
                       String -> n -> String -> String -> 
                       (TopicName, TopicType, a) -> IO ()
registerPublication name _n master uri (tname, ttype, _) = 
    do putStrLn $ "Registering publication of "++ttype++" on topic "++
                  tname++" on master "++master
       _subscribers <- registerPublisher master name tname ttype uri
       return ()

-- Inform the master that we are subscribing to a particular topic.
registerSubscription :: RosSlave n =>
                        String -> n -> String -> String -> 
                        (TopicName, TopicType, a) -> IO ()
registerSubscription name n master uri (tname, ttype, _) = 
    do putStrLn $ "Registring subscription to "++tname++" for "++ttype++"s"
       (r,_,publishers) <- registerSubscriber master name tname ttype uri
       if r == 1 
         then publisherUpdate n tname publishers
         else error "Failed to register subscriber with master"
       return ()

registerNode :: RosSlave s => String -> s -> IO ()
registerNode name n = 
    do uri <- readMVar (getNodeURI n)
       let master = getMaster n
       putStrLn $ "Starting node "++name++" at " ++ uri
       getPublications n >>= mapM_ (registerPublication name n master uri)
       getSubscriptions n >>= mapM_ (registerSubscription name n master uri)

-- |Run a ROS Node with the given name. Returns when the Node has
-- shutdown either by receiving an interrupt signal (e.g. Ctrl-C) or
-- because the master told it to stop.
runNode :: RosSlave s => String -> s -> IO ()
runNode name s = do (wait, _port) <- runSlave s
                    registerNode name s
                    putStrLn "Spinning"
                    allDone <- newQSem 0
                    let shutdown = do putStrLn "Shutting down"
                                      cleanupNode s `catch` \_ -> return ()
                                      signalQSem allDone
                    setShutdownAction s shutdown
                    _ <- installHandler sigINT (CatchOnce shutdown) Nothing
                    t <- forkIO $ wait >> signalQSem allDone
                    waitQSem allDone
                    killThread t `catch` \_ -> return ()
