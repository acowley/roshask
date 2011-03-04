-- |Utilities for working with ROS time values.
module Ros.Core.RosTime (ROSTime, ROSDuration, toROSTime, fromROSTime, 
                         diffROSTime, getROSTime) where
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX
import Ros.Core.RosTypes

toROSTime :: UTCTime -> ROSTime
toROSTime = aux . properFraction . utcTimeToPOSIXSeconds
  where aux (s,f) = (s, truncate $ f * 1000000)

-- |Class of types that may be derived from a 'ROSTime'.
class FromROSTime a where
  fromROSTime :: ROSTime -> a

instance FromROSTime UTCTime where
  fromROSTime = posixSecondsToUTCTime . aux . fromROSTime
    where aux = realToFrac :: Double -> NominalDiffTime

-- |Convert a 'ROSTime' to the POSIX number of seconds since epoch.
instance FromROSTime Double where
  fromROSTime (s,ns) = s' + ns'
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