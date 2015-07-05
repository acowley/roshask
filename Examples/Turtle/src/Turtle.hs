{-# LANGUAGE TemplateHaskell #-}
module Turtle (main) where
import Ros.Node
import Ros.Topic (cons, repeatM)
import Ros.Topic.Util (tee, filterBy, everyNew, interruptible, gate, share)
import Ros.Turtlesim.Pose
import Ros.Geometry_msgs.Twist
import qualified Ros.Geometry_msgs.Vector3 as V
import Ros.Logging

import System.IO (hFlush, stdout)
import Data.Default.Generics (def)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Lens.Family ((.~), (^.), (&))
import Data.Complex

-- A type synonym for a 2D point.
type Point = Complex Float

-- A Topic of user-supplied waypoint trajectories.
getTraj :: Topic IO [Point]
getTraj = repeatM (do putStr "Enter waypoints: " >> hFlush stdout
                      $(logInfo "Waiting for new traj")
                      (map (uncurry (:+)) . read) `fmap` getLine)

-- Create Twist message with linear and angular velocity in arguments.
mkTwist :: Real a => a -> a -> Twist
mkTwist l a = def & linear  . V.x .~ realToFrac l
                  & angular . V.z .~ realToFrac a

wrapAngle theta 
  | theta < 0    = theta + 2 * pi
  | theta > 2*pi = theta - 2 * pi
  | otherwise    = theta

angleDiff x y = wrapAngle (x' + pi - y') - pi
  where x' = if x < 0 then x + 2*pi else x
        y' = if y < 0 then y + 2*pi else y

-- Produce a unit value every time a goal is reached.
arrivalTrigger :: Topic IO Point -> Topic IO Pose -> Topic IO ()
arrivalTrigger goals poses = const () <$>
                             filterBy (arrived <$> goals) (p2v <$> poses)
  where arrived goal pose = magnitude (goal - pose) < 1.5
        p2v (Pose x y _ _ _) = x :+ y

-- Navigate to a goal given a current pose estimate.
navigate :: (Point, Pose) -> Twist
navigate (goal, pos) = mkTwist (min 2 (magnitude v)) angVel
  where v        = goal - (pos ^. x :+ pos ^. y)
        thetaErr = (angleDiff (phase v) (pos ^. theta)) * (180 / pi)
        angVel   = signum thetaErr * (min 2 (abs thetaErr))

main = runNode "HaskellBTurtle" $
       do enableLogging (Just Warn)
          poses <- subscribe "/turtle1/pose"
          (t1,t2) <- liftIO . tee . interruptible $ getTraj
          let goals = gate t1 (cons () (arrivalTrigger t2 poses))
          advertise "/turtle1/cmd_vel" $
                    fmap navigate (everyNew goals poses)
