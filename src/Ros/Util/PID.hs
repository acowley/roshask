-- |Basic PID control.
module Ros.Util.PID where
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- |A simple PID transfer function that assumes a unit sampling
-- interval. The first three parameters are the gains, the fourth
-- parameter is the desired setpoint, the fifth and sixth parameters
-- are the previous two errors, the seventh parameter is the most
-- recent system output. The return value is a tuple of the most
-- recent error and the computed controller output.
pidUniform :: Fractional a => a -> a -> a -> a -> a -> a -> a -> (a, a)
pidUniform kp ki kd obj = pidFixed kp ki kd obj 1
{-# INLINE pidUniform #-}

-- |PID controller with a fixed time interval between samples.
pidFixed :: Fractional a => a -> a -> a -> a -> a -> a -> a -> a -> (a,a)
pidFixed kp ki kd obj dt e1 e2 x = (e3, output)
  where e3 = x - obj
        invDt = 1 / dt
        scale = dt / 3
        integral = scale * (e1 + 4 * e2 + e3)
        derivative = (e3 - e2) * invDt
        output = kp * e3 + ki * integral + kd * derivative
{-# INLINE pidFixed #-}

-- |PID controller with explicit time stamps associated with each
-- sample. The order of the resultant tuples is (timeStamp, sample).
pidTimed :: Fractional a => a -> a -> a -> a -> (a,a) -> (a,a) -> (a,a) -> (a,a)
pidTimed kp ki kd obj (t1,e1) (_,e2) (t3,x) = (e3, output)
  where e3 = x - obj
        scale = (t3 - t1) / 6
        integral = scale * (e1 + 4 * e2 + e3)
        derivative = e3 - e2
        output = kp * e3 + ki * integral + kd * derivative
{-# INLINE pidTimed #-}

-- |A PID controller that maintains its own state. The first three
-- parameters are the gains, the fourth parameter is the desired
-- setpoint. The return value is an IO function that takes the newest
-- system output and returns the controller output.
pidFixedIO :: Fractional a => a -> a -> a -> a -> IO (a -> a -> IO a)
pidFixedIO kp ki kd dt = 
  do e1 <- newIORef 0
     e2 <- newIORef 0
     initialized <- newIORef (0::Int)
     return $ \setpoint -> 
       let pid' = pidFixed kp ki kd setpoint dt
       in \x -> 
         do init' <- readIORef initialized
            case init' of
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

-- |A PID controller that assumes a uniform sampling interval of 1.
pidUniformIO :: Fractional a => a -> a -> a -> IO (a -> a -> IO a)
pidUniformIO kp ki kd = pidFixedIO kp ki kd 1

-- |A PID controller that uses the system clock to associate a
-- timestamp with each measurement that then used to determine the
-- sampling interval.
pidTimedIO :: Fractional a => a -> a -> a -> IO (a -> a -> IO a)
pidTimedIO kp ki kd =
  do go <- pidWithTimeIO kp ki kd
     start <- getCurrentTime
     return $ \setpoint -> \x ->
       do t <- fmap (realToFrac . flip diffUTCTime start) getCurrentTime
          go setpoint (t,x)

-- |A PID controller that takes values of the form (timeStamp, sample)
-- such that the associated timestamp is used to determine the
-- sampling rate.
pidWithTimeIO :: Fractional a => a -> a -> a -> IO (a -> (a,a) -> IO a)
pidWithTimeIO kp ki kd =
  do e1 <- newIORef undefined
     e2 <- newIORef undefined
     initialized <- newIORef (0::Int)
     return $ \setpoint ->
       let pid' = pidTimed kp ki kd setpoint
       in \(t,x) ->
         do init' <- readIORef initialized
            case init' of
              0 -> do writeIORef e1 (t, x - setpoint)
                      writeIORef initialized 1
                      return 0
              1 -> do writeIORef e2 (t, x - setpoint)
                      writeIORef initialized 2
                      return 0
              _ -> do e1' <- readIORef e1
                      e2' <- readIORef e2
                      let (e3,c) = pid' e1' e2' (t,x)
                      writeIORef e1 e2'
                      e3 `seq` writeIORef e2 (t,e3)
                      return c
                           
{-# SPECIALIZE
  pidUniformIO :: Double -> Double -> Double -> IO (Double -> Double -> IO Double)
  #-}

{-# SPECIALIZE
  pidUniformIO :: Float -> Float -> Float -> IO (Float -> Float -> IO Float)
  #-}

{-# SPECIALIZE
  pidFixedIO :: Double -> Double -> Double -> Double -> 
                IO (Double -> Double -> IO Double)
  #-}

{-# SPECIALIZE
  pidFixedIO :: Float -> Float -> Float -> Float -> 
                IO (Float -> Float -> IO Float)
  #-}

{-# SPECIALIZE
  pidTimedIO :: Double -> Double -> Double -> IO (Double -> Double -> IO Double)
  #-}

{-# SPECIALIZE
  pidTimedIO :: Float -> Float -> Float -> IO (Float -> Float -> IO Float)
  #-}
