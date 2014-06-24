-- |PID related functions for 'Topic's.
module Ros.Topic.PID where
import Control.Applicative
import Ros.Topic
import Ros.Topic.Util
import qualified Ros.Util.PID as P

-- |@pidUniform2 kp ki kd setpoint t@ runs a PID controller that
-- transforms 'Topic' @t@ of process outputs into a 'Topic' of control
-- signals designed to steer the output to the setpoints produced by
-- 'Topic' @setpoint@ using the PID gains @kp@, @ki@, and @kd@. The
-- interval between samples produced by the 'Topic' is assumed to be
-- 1.
pidUniform2 :: Fractional a => a -> a -> a -> Topic IO a -> Topic IO a -> 
               Topic IO a
pidUniform2 kp ki kd setpoint t = 
  Topic $ do controller <- uncurry <$> P.pidUniformIO kp ki kd
             runTopic . join $ controller <$> everyNew setpoint t

-- |@pidUniform kp ki kd setpoint t@ runs a PID controller that
-- transforms 'Topic' @t@ of process outputs into a 'Topic' of control
-- signals designed to steer the output to the given setpoint using
-- the PID gains @kp@, @ki@, and @kd@. The interval between samples
-- produced by the 'Topic' is assumed to be 1.
pidUniform :: Fractional a => a -> a -> a -> a -> Topic IO a -> Topic IO a
pidUniform kp ki kd setpoint t = 
  Topic $ do controller <- ($ setpoint) <$> P.pidUniformIO kp ki kd
             runTopic . join $ controller <$> t

-- |@pidFixed2 kp ki kd setpoint dt t@ runs a PID controller that
-- transforms 'Topic' @t@ of process outputs into a 'Topic' of control
-- signals designed to steer the output to the setpoints produced by
-- 'Topic' @setpoint@ using the PID gains @kp@, @ki@, and @kd@, along
-- with an assumed fixed time interval, @dt@, between samples.
pidFixed2 :: Fractional a => a -> a -> a -> a -> Topic IO a -> Topic IO a -> 
             Topic IO a
pidFixed2 kp ki kd dt setpoint t = 
  Topic $ do controller <- uncurry <$> P.pidFixedIO kp ki kd dt
             runTopic . join $ controller <$> everyNew setpoint t


-- |@pidFixed kp ki kd setpoint dt t@ runs a PID controller that
-- transforms 'Topic' @t@ of process outputs into a 'Topic' of control
-- signals designed to steer the output to the given setpoint using
-- the PID gains @kp@, @ki@, and @kd@, along with an assumed fixed
-- time interval, @dt@, between samples.
pidFixed :: Fractional a => a -> a -> a -> a -> a -> Topic IO a -> Topic IO a
pidFixed kp ki kd setpoint dt t = 
  Topic $ do controller <- ($ setpoint) <$> P.pidFixedIO kp ki kd dt
             runTopic . join $ controller <$> t

-- |@pidTimed2 kp ki kd setpoint t@ runs a PID controller that
-- transforms 'Topic' @t@ of process outputs into a 'Topic' of control
-- signals designed to steer the output to the setpoints produced by
-- 'Topic' @setpoint@ using the PID gains @kp@, @ki@, and @kd@. The
-- system clock is checked for each value produced by the input
-- 'Topic' to determine the actual sampling rate.
pidTimed2 :: Fractional a => a -> a -> a -> Topic IO a -> Topic IO a -> 
             Topic IO a
pidTimed2 kp ki kd setpoint t = 
  Topic $ do controller <- uncurry <$> P.pidTimedIO kp ki kd
             runTopic . join $ controller <$> everyNew setpoint t

-- |@pidTimed kp ki kd setpoint t@ runs a PID controller that
-- transforms 'Topic' @t@ of process outputs into a 'Topic' of control
-- signals designed to steer the output to the given setpoint using
-- the PID gains @kp@, @ki@, and @kd@. The system clock is checked for
-- each value produced by the input 'Topic' to determine the actual
-- sampling rate.
pidTimed :: Fractional a => a -> a -> a -> a -> Topic IO a -> Topic IO a
pidTimed kp ki kd setpoint t = 
  Topic $ do controller <- ($ setpoint) <$> P.pidTimedIO kp ki kd
             runTopic . join $ controller <$> t


-- |@pidStamped2 kp ki kd setpoint t@ runs a PID controller that
-- transforms 'Topic' @t@ of process outputs into a 'Topic' of control
-- signals designed to steer the output to the given setpoint using
-- the PID gains @kp@, @ki@, and @kd@. Values produced by the 'Topic'
-- @t@ must be paired with a timestamp and thus have the form
-- (timeStamp, sample).
pidStamped2 :: Fractional a => a -> a -> a -> Topic IO a -> Topic IO (a,a) -> 
               Topic IO a
pidStamped2 kp ki kd setpoint t =
  Topic $ do controller <- uncurry <$> P.pidWithTimeIO kp ki kd
             runTopic . join $ controller <$> everyNew setpoint t

-- |@pidStamped kp ki kd setpoint t@ runs a PID controller that
-- transforms 'Topic' @t@ of process outputs into a 'Topic' of control
-- signals designed to steer the output to the given setpoint using
-- the PID gains @kp@, @ki@, and @kd@. Values produced by the 'Topic'
-- @t@ must be paired with a timestamp and thuse have the form
-- (timeStamp, sample).
pidStamped :: Fractional a => a -> a -> a -> a -> Topic IO (a,a) -> Topic IO a
pidStamped kp ki kd setpoint t =
  Topic $ do controller <- ($ setpoint) <$> P.pidWithTimeIO kp ki kd
             runTopic . join $ controller <$> t
