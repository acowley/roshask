module Tests.TopicInterp where
import Control.Arrow
import Control.Concurrent
import Ros.Topic
import Ros.TopicStamped
import Ros.Core.Header
import qualified Ros.Core.Header as H
import Ros.Core.Msg.HeaderSupport
import Ros.Core.RosTime
import Debug.Trace

newtype CharMsg = CharMsg {unCM :: (Char, Header)}

instance HasHeader CharMsg where
  getSequence = H.seq . snd . unCM
  getStamp = stamp . snd . unCM
  getFrame = undefined
  setSequence = undefined

newtype FloatMsg = FloatMsg {unFM :: (Float, Header)}

instance HasHeader FloatMsg where
  getSequence = H.seq . snd . unFM
  getStamp = stamp . snd . unFM
  getFrame = undefined
  setSequence = undefined

alphabet :: Topic IO CharMsg
alphabet = go 'A' 0
  where go c i = Topic $ do threadDelay (truncate 1e5)
                            now <- getROSTime
                            let msg = CharMsg (c, Header i now "")
                            return (msg, go (succ c) (i+1))

positions :: Topic IO FloatMsg
positions = cons f0 $ go 1 1
  where go p i = Topic $ do threadDelay (truncate 1e6)
                            now <- getROSTime
                            let msg = FloatMsg (p, Header i now "")
                            return (msg, go (p+10) (i+1))
        f0 = FloatMsg (0, Header 0 (0,0) "")

test :: IO ()
test = do t1 <- runTopicThread positions
          t2 <- runTopicThread alphabet
          forever . showTopic . fmap (getf *** getc) $ interpolate f t1 t2
  where f (FloatMsg (x,h)) (FloatMsg (y,_)) alpha =
          trace ("Interpolate "++show x++" -> "++show y++" @ "++show alpha) $
          FloatMsg ((y - x) * realToFrac alpha + x, h)
        getf (FloatMsg (f,_)) = f
        getc (CharMsg (c,_)) = c

runTopicThread :: Topic IO a -> IO (Topic IO a)
runTopicThread t = do c <- newChan
                      forkIO . forever . join $ fmap (writeChan c) t
                      let feed = Topic $ 
                                 do x <- readChan c
                                    return (x, feed)
                      return feed
