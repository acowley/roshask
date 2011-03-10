{-# LANGUAGE TemplateHaskell #-}
module Turtle3 (main) where
import Prelude hiding (dropWhile)
import Control.Applicative
import Control.Arrow
import System.IO (hFlush, stdout)
import Data.VectorSpace
import Ros.Logging
import Ros.Node
import Ros.Topic (repeatM, force, dropWhile, metamorphM, yieldM, topicOn)
import Ros.TopicUtil (everyNew, interruptible)
import Ros.TopicPID (pidTimed)
import AngleNum
import Ros.Turtlesim.Pose
import Ros.Turtlesim.Velocity

-- A type synonym for a 2D point.
type Point = (Float,Float)

-- A Topic of user-supplied waypoint trajectories.
getTraj :: Topic IO [Point]
getTraj = repeatM (do putStr "Enter waypoints: " >> hFlush stdout
                      $(logInfo "Waiting for new traj")
                      read `fmap` getLine)

-- Produce a new goal 'Point' every time a 'Pose' topic reaches the
-- vicinity of the previous goal.
destinations :: (Functor m, Monad m) => 
                Topic m Point -> Topic m Pose -> Topic m Point
destinations goals poses = metamorphM (start (p2v <$> poses)) goals
  where start t g = yieldM g (go g t)
        go g t g' = let keepGoing x = magnitude (g ^-^ x) > 1.5
                    in force (dropWhile keepGoing t) >>= yieldM g' . go g'
        p2v (Pose x y _ _ _) = (x, y)

-- Compute linear and angular error to goal.
toGoal :: (Pose,Point) -> (Float, Float)
toGoal (pos,goal) = (magnitude v, thetaErr)
  where v@(vx,vy) = goal ^-^ (x pos, y pos)
        thetaDesired = angle $ atan2 vy vx
        thetaErr = toDegrees $ thetaDesired - angle (theta pos)

-- Run a PID loop on angular velocity to converge to a bearing for the goal.
steering :: Topic IO (Float,Float) -> Topic IO Velocity
steering = topicOn snd (Velocity . fst) (pidTimed 1 0 0 0)

navigate :: Topic IO (Pose,Point) -> Topic IO Velocity
navigate = steering . fmap ((clamp***clamp) . toGoal)
  where clamp = min 2

main = runNode "HaskellBTurtle" $
       do enableLogging (Just Warn)
          poses <- subscribe "/turtle1/pose"
          let goals = destinations (interruptible getTraj) poses
          advertise "/turtle1/command_velocity" . navigate $ everyNew poses goals
