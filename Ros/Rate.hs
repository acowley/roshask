-- |Provides a rate limiting mechanism that can be used to control the
-- rate at which 'IO' actions produce values.
module Ros.Rate (rateLimiter) where
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

timeDiff :: UTCTime -> UTCTime -> Double
timeDiff = curry $ realToFrac . uncurry diffUTCTime

-- |Produces an action that runs the supplied 'IO' action no faster
-- than given rate in Hz.
rateLimiter :: Double -> IO a -> IO (IO a)
rateLimiter hz action = do prev <- getCurrentTime >>= newIORef
                           est <- newIORef period
                           return $ do t1 <- getCurrentTime
                                       t0 <- readIORef prev
                                       estimate <- readIORef est
                                       let error = timeDiff t1 t0 - period
                                           estimate' = estimate*0.8 + 
                                                       (estimate - error)*0.2
                                           delay = truncate $ estimate' * 1e6
                                       when (delay > 0) $ threadDelay delay
                                       x <- action
                                       writeIORef prev t1
                                       writeIORef est estimate'
                                       return x
    where period = 1 / hz
