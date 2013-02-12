module Tests.TopicInterp where
import Control.Arrow
import Control.Concurrent
import Ros.Topic
import Ros.TopicUtil (forkTopic)
import Ros.TopicStamped
import Ros.Internal.Header
import qualified Ros.Internal.Header as H
import Ros.Internal.Msg.HeaderSupport
import Ros.Internal.RosTime

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

-- The idea behind this test is that we have an agent moving along a
-- linear arrangement of the alphabet. The agent moves at 10
-- characters-per-second, but its position is only updated at 1Hz. We
-- must therefore interpolate its linear position to associate each
-- sensed character with its position.

-- A Topic that produces a new character at 10Hz
alphabet :: Topic IO CharMsg
alphabet = go 'A' 0
  where go c i = Topic $ do threadDelay (truncate 1e5)
                            now <- getROSTime
                            let msg = CharMsg (c, Header i now "")
                            return (msg, go (succ c) (i+1))

-- A Topic that produces a new "position" at 1Hz. The position is
-- incremented by 10 at each step to indicate a subsampling of true
-- position.
positions :: Topic IO FloatMsg
positions = go 10 1
  where go p i = Topic $ do threadDelay (truncate 1e6)
                            now <- getROSTime
                            let msg = FloatMsg (p, Header i now "")
                            return (msg, go (p+10) (i+1))

-- We prepend the initial position on the 'positions' Topic to ensure
-- that its time stamp preceeds any value produced by the 'alphabet'
-- Topic.
test :: IO ()
test = do now <- getROSTime
          let f0 = FloatMsg (0, Header 0 now "")
          t1 <- forkTopic (cons f0 positions)
          t2 <- forkTopic alphabet
          forever . showTopic . fmap (getf *** getc) $ interpolate f t1 t2
  where f (FloatMsg (x,h)) (FloatMsg (y,_)) alpha =
          FloatMsg ((y - x) * realToFrac alpha + x, h)
        getf (FloatMsg (f,_)) = f
        getc (CharMsg (c,_)) = c

