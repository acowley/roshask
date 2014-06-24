{-# LANGUAGE TupleSections #-}
module Ros.Topic.Stats (sendMessageStat, recvMessageStat, statSnapshot,
                        StatMap, SubStats(..), PubStats(..)) where
import Control.Applicative ((<$>))
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as M
import Ros.Internal.RosTypes

data SubStats = SubStats { bytesReceived  :: !Int
                         , subConnected   :: !Bool }

data PubStats = PubStats { bytesSent      :: !Int
                         , numSent        :: !Int
                         , pubConnected   :: !Bool }


-- |A transactional data store for tracking all the connections for a
-- particular topic.
type StatMap a = TVar (Map URI (TVar a))

statSnapshot :: (URI, TVar a) -> STM (URI, a)
statSnapshot (uri, stat) = (uri,) <$> readTVar stat

-- |Record the fact that we've sent a message of the given number of
-- bytes to the given URI. If the number of bytes is negative, the
-- connection is marked is disconnected.
sendMessageStat :: StatMap PubStats -> URI -> Int -> IO ()
sendMessageStat tm uri numBytes = 
    atomically $ do m <- readTVar tm
                    let conn = numBytes >= 0
                        nb = max 0 numBytes
                        nm = if conn then 1 else 0
                    case M.lookup uri m of
                      Nothing -> do stats <- newTVar $ PubStats nb nm conn
                                    writeTVar tm (M.insert uri stats m)
                      Just ts -> do PubStats nb' nm' _ <- readTVar ts
                                    let stats = PubStats (nb' + nb)
                                                         (nm' + nm)
                                                         conn
                                    writeTVar ts stats

-- |Record the fact that we've received a message of the given number
-- of bytes from the given URI. If the number of bytes is negative,
-- the connection is marked as disconnected.
recvMessageStat :: StatMap SubStats -> URI -> Int -> IO ()
recvMessageStat tm uri numBytes = 
    atomically $ do m <- readTVar tm
                    let conn = numBytes >= 0
                        nb = max 0 numBytes
                    case M.lookup uri m of
                      Nothing -> do stats <- newTVar $ SubStats nb conn
                                    writeTVar tm (M.insert uri stats m)
                      Just ts -> do SubStats nb' _ <- readTVar ts
                                    let stats = SubStats (nb' + nb) conn
                                    writeTVar ts stats
