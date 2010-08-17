module Ros.Rate (rateLimiter) where
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.CPUTime (getCPUTime)

rateLimiter :: Float -> IO a -> IO (IO a)
rateLimiter hz action = do prev <- newIORef 0
                           return $ do t1 <- getCPUTime
                                       x <- action
                                       t0 <- readIORef prev
                                       let dt = fromIntegral $
                                                (period - t1 + t0) `div` 1000000
                                       when (dt > 0) $ threadDelay dt
                                       writeIORef prev t1
                                       return x
    where period = round $ 1e12 / hz

--rateLimiter :: Float -> IO (IO ())
-- rateLimiter hz = do prev <- newIORef 0
--                     return $ do t1 <- getCPUTime
--                                 t0 <- readIORef prev
--                                 let dt = fromIntegral $
--                                          (period - t1 + t0) `div` 1000000
--                                 when (dt > 0) $ threadDelay dt
--                                 writeIORef prev t1
--     where period = round $ 1e12 / hz