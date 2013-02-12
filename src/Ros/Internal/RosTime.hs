-- |Utilities for working with ROS time values.
module Ros.Internal.RosTime (ROSTime, ROSDuration, toROSTime, fromROSTime, 
                             diffROSTime, getROSTime, diffSeconds) where
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX
import Data.Word (Word32)
import Ros.Internal.RosTypes

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
  | dns >= 0 = (fi ds, fi dns)
  | otherwise = (fi $ ds - 1, fi $ 1000000 + dns)
  where dns = fw ns1 - fw ns2
        ds = fw s1 - fw s2
        fw :: Word32 -> Int
        fw = fromIntegral
        fi :: Int -> Word32
        fi = fromIntegral

-- |Get the current POSIX time.
getROSTime :: IO ROSTime
getROSTime = fmap (aux . properFraction) getPOSIXTime
  where aux (s,f) = (s, truncate $ f * 1000000)

-- |Compute the difference in seconds between two 'ROSTime'
-- values. The application @diffSeconds tStop tStart@ computes the
-- time interval @tStop - tStart@.
diffSeconds :: ROSTime -> ROSTime -> Double
diffSeconds t1 t2 = fromROSTime $ diffROSTime t1 t2