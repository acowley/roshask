-- |Utilities for working with ROS time values.
module Ros.Core.RosTime (ROSTime, ROSDuration, toROSTime, fromROSTime, 
                         diffROSTime, getROSTime) where
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX
import Ros.Core.RosTypes

toROSTime :: UTCTime -> ROSTime
toROSTime = aux . properFraction . utcTimeToPOSIXSeconds
  where aux (s,f) = (s, truncate $ f * 1000000)

fromROSTime :: ROSTime -> UTCTime
fromROSTime (s,ns) = posixSecondsToUTCTime . realToFrac $ s' + ns'
  where s' = fromIntegral s             :: Double
        ns' = fromIntegral ns / 1000000 :: Double

-- |@timeDiff t1 t2@ computes the difference @t1 - t2@.
diffROSTime :: ROSTime -> ROSTime -> ROSDuration
diffROSTime (s1,ns1) (s2,ns2)
  | dns >= 0 = (ds, dns)
  | otherwise = (ds - 1, 1000000 - dns)
  where dns = ns1 - ns2
        ds = s1 - s2

-- |Get the current POSIX time.
getROSTime :: IO ROSTime
getROSTime = fmap (aux . properFraction) getPOSIXTime
  where aux (s,f) = (s, truncate $ f * 1000000)