-- |Binary iteratee-style serialization helpers for working with ROS
-- message types. This module is used by the automatically-generated
-- code for ROS .msg types.
module Ros.Node.BinaryIter (streamIn, getServiceResult) where
import Control.Applicative
import Control.Concurrent (myThreadId, killThread)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.IO (Handle)
import Ros.Topic
import Ros.Internal.RosBinary (RosBinary(get))
import Ros.Service.ServiceTypes(ServiceResponseExcept(..))
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Monad.Except (ExceptT(..), throwError)

-- Get the specified number of bytes from a 'Handle'. Returns a
-- wrapped-up 'Nothing' if the client shutdown (indicated by receiving
-- a message of zero length).
hGetAll :: Handle -> Int -> MaybeT IO BL.ByteString
hGetAll h n = go n []
    where go 0 acc = return . BL.fromChunks $ reverse acc
          go n' acc = do bs <- liftIO $ BS.hGet h n'
                         case BS.length bs of
                           0 -> MaybeT $ return Nothing
                           x -> go (n' - x) (bs:acc)

-- |The function that does the work of streaming members of the
-- 'RosBinary' class in from a 'Handle'.
streamIn :: RosBinary a => Handle -> Topic IO a
streamIn h = Topic go 
  where go = do item <- runMaybeT $ do len <- runGet getInt <$> hGetAll h 4
                                       runGet get <$> hGetAll h len
                case item of
                  Nothing -> putStrLn "Publisher stopped" >>
                             myThreadId >>= killThread >>
                             return undefined
                  Just item' -> return (item', Topic go)

getInt :: Get Int
getInt = fromIntegral <$> getWord32le

-- | Get the result back from a service call (called by the service client)
-- (see http://wiki.ros.org/ROS/TCPROS)
getServiceResult :: RosBinary a => Handle ->  ExceptT ServiceResponseExcept IO a
getServiceResult h = do
  okByte <- runGet getWord8 <$> hGetAllET h 1 (ResponseReadExcept "Could not read okByte")
  case okByte of
    0 -> do
      len <- runGet getInt <$> hGetAllET h 4 (ResponseReadExcept "Could not read length for notOk message")
      message <- hGetAllET h len (ResponseReadExcept "Could not read notOk message")
      throwError . NotOkExcept $ unpack message
    _ -> do
      len <- runGet getInt <$> hGetAllET h 4 (ResponseReadExcept "Could not read length")
      runGet get <$> hGetAllET h len (ResponseReadExcept "Could not read response message")
  
hGetAllET ::  Handle -> Int -> ServiceResponseExcept -> ExceptT ServiceResponseExcept IO BL.ByteString
hGetAllET h n exceptMessage = do
  maybeData <- liftIO . runMaybeT $ hGetAll h n
  case maybeData of
    Nothing -> throwError exceptMessage
    Just b -> ExceptT . return $ Right b
