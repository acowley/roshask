module Tests.TopicWindow where
import Prelude hiding (mapM)
import Control.Applicative
import Control.Monad (join)
import Data.Monoid
import Ros.Rate
import Ros.Topic hiding (join)
import Ros.TopicUtil
import Data.Time.Clock (utctDayTime, getCurrentTime)
import System.Random

currentMinute :: Topic IO Int
currentMinute = repeatM . join . rateLimiter 1 $
                toSeconds . utctDayTime <$> getCurrentTime
  where toSeconds = (`rem` 60) . (`div` 60) . truncate . toRational

addNoise :: Int -> Topic IO Int -> Topic IO Int
addNoise m = mapM ((<$> getStdRandom (randomR (-m, m))) . (+))

-- Monoid on Int with (+)
test1 = forever . showTopic . fmap ((`div` 10) . getSum) $
        slidingWindow 10 (Sum <$> addNoise 3 currentMinute)

-- Int is an additive group
test2 = forever . showTopic . fmap (`div` 10) $
        slidingWindowG 10 (addNoise 3 currentMinute)

addNoiseF :: Float -> Topic IO Int -> Topic IO Float
addNoiseF m = mapM ((<$> getStdRandom (randomR (-m,m))) . (+) . fromIntegral)

test3 = forever . showTopic . fmap (/ 10) $
        slidingWindowG 10 (addNoiseF 3 currentMinute)