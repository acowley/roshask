-- |Binary iteratee-style serialization helpers for working with ROS
-- message types. This module is used by the automatically-generated
-- code for ROS .msg types.
module Ros.Node.BinaryIter (streamIn) where
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
