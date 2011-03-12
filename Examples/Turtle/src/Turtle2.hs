{-# LANGUAGE TemplateHaskell #-}
module Turtle2 (main) where
import Prelude hiding (dropWhile)
import Control.Applicative
import Control.Arrow
import Data.VectorSpace
import AngleNum
import Ros.Node
import Ros.Topic (repeatM, force, dropWhile, metamorphM, yieldM)
import Ros.TopicUtil (everyNew, interruptible)
import Ros.Turtlesim.Pose
import Ros.Turtlesim.Velocity
import Ros.Logging
import System.IO (hFlush, stdout)

-- A type synonym for a 2D point.
type Point = (Float,Float)

-- A Topic of user-supplied waypoint trajectories.
getTraj :: Topic IO [Point]
getTraj = repeatM (do putStr "Enter waypoints: " >> hFlush stdout
                      $(logInfo "Waiting for new traj")
                      read <$> getLine)

-- Produce a new goal 'Point' every time a goal is reached.
destinations :: (Functor m, Monad m) => 
                Topic m Point -> Topic m Pose -> Topic m Point
destinations goals poses = metamorphM (start (p2v <$> poses)) goals
  where start t g = yieldM g (go g t)
        go g t g' = force (dropWhile (keepGoing g) t) >>= yieldM g' . go g'
        keepGoing goal pose = magnitude (goal ^-^ pose) > 1.5
        p2v (Pose x y _ _ _) = (x, y)

-- Compute linear distance to goal and bearing to goal
toGoal :: (Pose,Point) -> (Float, Angle Float)
toGoal (pos,goal) = (magnitude v, angle $ atan2 vy vx)
  where v@(vx,vy) = goal ^-^ (x pos, y pos)

-- Steer based on a current pose estimate and distance from goal
steering :: Pose -> (Float, Angle Float) -> Velocity
steering pos (dpos, thetaDesired) = Velocity (min 2 dpos) angVel
  where thetaErr = toDegrees $ thetaDesired - angle (theta pos)
        angVel = signum thetaErr * min 2 (abs thetaErr)

navigate :: (Pose, Point) -> Velocity
navigate = uncurry ($) . (steering . fst &&& toGoal)

main = runNode "HaskellBTurtle" $
       do enableLogging (Just Warn)
          poses <- subscribe "/turtle1/pose"
          let goals = destinations (interruptible getTraj) poses
          advertise "/turtle1/command_velocity" 
                    (navigate <$> everyNew poses goals)
