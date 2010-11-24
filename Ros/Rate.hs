-- |Provides a rate limiting mechanism that can be used to control the
-- rate at which 'IO' actions produce values.
module Ros.Rate (rateLimiter, pid, pidIO) where
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Ros.Util.PID

timeDiff :: UTCTime -> UTCTime -> Double
timeDiff = curry $ realToFrac . uncurry diffUTCTime

-- |Produces an action that runs the supplied 'IO' action no faster
-- than given rate in Hz.
rateLimiter :: Double -> IO a -> IO (IO a)
rateLimiter hz action = do control <- pidIO (-0.2) (-0.1) (-0.1) period
                           prevDelay <- newIORef period
                           prevTime <- getCurrentTime >>= newIORef
                           return $ do t1 <- getCurrentTime
                                       t0 <- readIORef prevTime
                                       let tdiff = timeDiff t1 t0 * 1000000
                                       change <- control tdiff
                                       delay <- readIORef prevDelay
                                       let delay' = delay + change
                                       when (delay' > 0)
                                            (threadDelay $ truncate delay')
                                       x <- action
                                       writeIORef prevDelay delay'
                                       writeIORef prevTime t1
                                       return x
  where period = 1000000 / hz
