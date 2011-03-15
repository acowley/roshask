{-# LANGUAGE TemplateHaskell #-}
module Turtle3 (main) where
import Prelude hiding (dropWhile)
import Control.Applicative
import Control.Arrow
import System.IO (hFlush, stdout)
import Data.Complex
import Ros.Logging
import Ros.Node
import Ros.Topic (repeatM, force, dropWhile, metamorphM, yieldM)
import Ros.TopicUtil (everyNew, interruptible, forkTopic, topicOn)
import Ros.Util.PID (pidTimedIO)
import AngleNum
import Ros.Turtlesim.Pose
import Ros.Turtlesim.Velocity

-- A type synonym for a 2D point.
type Point = Complex Float

-- A Topic of user-supplied waypoint trajectories.
getTraj :: Topic IO [Point]
getTraj = repeatM (do putStr "Enter waypoints: " >> hFlush stdout
                      $(logInfo "Waiting for new traj")
                      map (uncurry (:+)) . read <$> getLine)

-- Produce a new goal 'Point' every time a 'Pose' topic reaches the
-- vicinity of the previous goal.
destinations :: (Functor m, Monad m) => 
                Topic m Point -> Topic m Pose -> Topic m Point
destinations goals poses = metamorphM (start (p2v <$> poses)) goals
  where start t g = yieldM g (go g t)
        go g t g' = let keepGoing x = magnitude (g - x) > 1.5
                    in force (dropWhile keepGoing t) >>= yieldM g' . go g'
        p2v (Pose x y _ _ _) = x :+ y

-- Compute linear and angular error to goal.
toGoal :: (Pose,Point) -> (Float, Float)
toGoal (pos,goal) = (magnitude v, thetaErr)
  where v = goal - (x pos :+ y pos)
        thetaErr = toDegrees $ angle (phase v) - angle (theta pos)

-- Run a PID loop on angular velocity to converge to a bearing for the goal.
steering :: Topic IO (Float,Float) -> Topic IO Velocity
steering = topicOn snd (Velocity . fst) (($ 0) <$> pidTimedIO 1 0 0)

-- Compute position and bearing error to goal, clamp linear and
-- angular velocities, and generate a steering command.
navigate :: Topic IO (Pose,Point) -> Topic IO Velocity
navigate = steering . fmap ((clamp *** clamp) . toGoal)
  where clamp x = signum x * min 2 (abs x)

main = runNode "HaskellBTurtle" $
       do enableLogging (Just Warn)
          poses <- subscribe "/turtle1/pose"
          let goals = destinations (interruptible getTraj) poses
              commands =  navigate $ everyNew poses goals
          advertise "/turtle1/command_velocity" commands
