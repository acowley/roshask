-- |Basic PID control.
module Ros.Util.PID where
import Data.IORef (newIORef, readIORef, writeIORef)

-- |A simple PID transfer function. The first three parameters are the
-- gains, the fourth parameter is the desired setpoint, the fifth and
-- sixth parameters are the previous two errors, the seventh parameter
-- is the most recent system output. The return value is a tuple of
-- the most recent error and the computed controller output.
pid :: Fractional a => a -> a -> a -> a -> a -> a -> a -> (a, a)
pid kp ki kd obj e1 e2 x = (e3, kp * e3 + ki * integral + kd * derivative)
  where e3 = x - obj
        integral = (e1 + 4 * e2 + e3) / 3
        derivative = e3 - e2

{-# SPECIALIZE 
  pid :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> 
         (Double, Double)
  #-}

{-# SPECIALIZE 
  pid :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> 
         (Float, Float)
  #-}

-- |A PID controller that maintains its own state. The first three
-- parameters are the gains, the fourth parameter is the desired
-- setpoint. The return value is an IO function that takes the newest
-- system output and returns the controller output.
pidIO :: Fractional a => a -> a -> a -> a -> IO (a -> IO a)
pidIO kp ki kd setpoint = do e1 <- newIORef 0
                             e2 <- newIORef 0
                             initialized <- newIORef (0::Int)
                             return $ \x -> 
                               do init <- readIORef initialized
                                  case init of
                                    0 -> do writeIORef e1 (x - setpoint)
                                            writeIORef initialized 1
                                            return 0
                                    1 -> do writeIORef e2 (x - setpoint)
                                            writeIORef initialized 2
                                            return 0
                                    _ -> do e1' <- readIORef e1
                                            e2' <- readIORef e2
                                            let (e3,c) = pid' e1' e2' x
                                            writeIORef e1 e2'
                                            e3 `seq` writeIORef e2 e3
                                            return c
  where pid' = pid kp ki kd setpoint

{-# SPECIALIZE
  pidIO :: Double -> Double -> Double -> Double -> IO (Double -> IO Double)
  #-}

{-# SPECIALIZE
  pidIO :: Float -> Float -> Float -> Float -> IO (Float -> IO Float)
  #-}
