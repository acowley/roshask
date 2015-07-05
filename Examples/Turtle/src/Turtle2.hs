{-# LANGUAGE TemplateHaskell #-}
module Turtle2 (main) where
import Prelude hiding (dropWhile)
import Control.Applicative
import Control.Arrow
import Data.Complex
import AngleNum
import Ros.Node
import Ros.Topic (repeatM, force, dropWhile, metamorphM, yieldM)
import Ros.Topic.Util (everyNew, interruptible)
import Ros.Turtlesim.Pose
import Ros.Geometry_msgs.Twist
import qualified Ros.Geometry_msgs.Vector3 as V
import Ros.Logging

import System.IO (hFlush, stdout)
import Data.Default.Generics (def)
import Lens.Family ((.~), (^.), (&))
import Control.Arrow ((&&&))

-- A type synonym for a 2D point.
type Point = Complex Float

-- A Topic of user-supplied waypoint trajectories.
getTraj :: Topic IO [Point]
getTraj = repeatM (do putStr "Enter waypoints: " >> hFlush stdout
                      $(logInfo "Waiting for new traj")
                      map (uncurry (:+)) . read <$> getLine)

-- Create Twist message with linear and angular velocity in arguments.
mkTwist :: Real a => a -> a -> Twist
mkTwist l a = def & linear  . V.x .~ realToFrac l
                  & angular . V.z .~ realToFrac a

-- Produce a new goal 'Point' every time a goal is reached.
destinations :: (Functor m, Monad m) => 
                Topic m Point -> Topic m Pose -> Topic m Point
destinations goals poses = metamorphM (start (p2v <$> poses)) goals
  where start t g = yieldM g (go g t)
        go g t g' = force (dropWhile (keepGoing g) t) >>= yieldM g' . go g'
        keepGoing goal pose = magnitude (goal - pose) > 1.5
        p2v (Pose x y _ _ _) = x :+ y

-- Compute linear distance to goal and bearing to goal
toGoal :: (Pose,Point) -> (Float, Angle Float)
toGoal (pos,goal) = (magnitude v, angle $ phase v)
  where v = goal - (pos^.x :+ pos^.y)

-- Steer based on a current pose estimate and distance from goal
steering :: Pose -> (Float, Angle Float) -> Twist
steering pos (dpos, thetaDesired) = mkTwist (min 2 dpos) angVel
  where thetaErr = toDegrees $ thetaDesired - angle (pos^.theta)
        angVel = signum thetaErr * min 2 (abs thetaErr)

navigate :: (Pose, Point) -> Twist
navigate = uncurry ($) . (steering . fst &&& toGoal)

main = runNode "HaskellBTurtle" $
       do enableLogging (Just Warn)
          poses <- subscribe "/turtle1/pose"
          let goals = destinations (interruptible getTraj) poses
          advertise "/turtle1/cmd_vel" 
                    (navigate <$> everyNew poses goals)
