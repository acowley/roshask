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
import Ros.Topic.Util (everyNew, interruptible, forkTopic, topicOn, subsample)
import Ros.Util.PID (pidTimedIO)
import AngleNum
import Ros.Turtlesim.Pose
import Ros.Geometry_msgs.Twist
import qualified Ros.Geometry_msgs.Vector3 as V
import Data.Default.Generics (def)
import Lens.Family ((.~), (^.), (&))

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

-- Produce a new goal 'Point' every time a 'Pose' topic reaches the
-- vicinity of the previous goal.
destinations :: (Functor m, Monad m) => 
                Topic m Point -> Topic m Pose -> Topic m Point
destinations goals poses = metamorphM (start (p2v <$> poses)) goals
  where start t g = yieldM g (go g t)
        go g t g' = let keepGoing x = magnitude (g - x) > 0.5
                    in force (dropWhile keepGoing t) >>= yieldM g' . go g'
        p2v (Pose x y _ _ _) = x :+ y

-- Compute linear and angular error to goal.
toGoal :: (Pose,Point) -> (Float, Float)
toGoal (pos,goal) = (magnitude v, thetaErr)
  where v = goal - (pos ^. x :+ pos ^. y)
        thetaErr = toDegrees $ angle (phase v) - angle (pos ^. theta)

-- Run a PID loop on angular velocity to converge to a bearing for the
-- goal.
steering :: Topic IO (Float,Float) -> Topic IO Twist
steering = topicOn snd (mkTwist . fst) (($ 0) <$> pidTimedIO 0.01 0.5 0)

-- Compute position and bearing error to goal, clamp linear and
-- angular velocities, and generate a steering command.
navigate :: Topic IO (Pose,Point) -> Topic IO Twist
navigate = steering . fmap ((clamp *** id) . toGoal)
  where clamp x = signum x * min 2 (abs x)

main = runNode "HaskellBTurtle" $
       do enableLogging (Just Warn)
          poses <- subscribe "/turtle1/pose"
          let goals = destinations (interruptible getTraj) poses
              commands = subsample 15 $ navigate $ everyNew poses goals
          advertise "/turtle1/cmd_vel" commands
