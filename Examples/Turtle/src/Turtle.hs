{-# LANGUAGE TemplateHaskell #-}
module Turtle (main) where
import Data.VectorSpace
import Ros.Node
import Ros.Topic (cons, repeatM)
import Ros.TopicUtil (tee, filterBy, everyNew, interruptible, gate)
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
                      read `fmap` getLine)

wrapAngle theta 
  | theta < 0    = theta + 2 * pi
  | theta > 2*pi = theta - 2 * pi
  | otherwise    = theta

angleDiff x y = wrapAngle (x' + pi - y') - pi
  where x' = if x < 0 then x + 2*pi else x
        y' = if y < 0 then y + 2*pi else y

-- Produce a unit value every time a goal is reached.
arrivalTrigger :: Topic IO Point -> Topic IO Pose -> Topic IO ()
arrivalTrigger goals poses = fmap (const ()) $
                             filterBy (fmap arrived goals) (fmap p2v poses)
  where arrived goal pose = magnitude (goal ^-^ pose) < 1.5
        p2v (Pose x y _ _ _) = (x, y)

-- Navigate to a goal given a current pose estimate.
navigate :: (Point, Pose) -> Velocity
navigate (goal, pos) = Velocity (min 2 (magnitude v)) angVel
  where v@(vx, vy)   = goal ^-^ (x pos, y pos)
        thetaDesired = atan2 vy vx
        thetaErr     = (angleDiff thetaDesired (theta pos)) * (180 / pi)
        angVel       = signum thetaErr * (min 2 (abs thetaErr))

main = runNode "HaskellBTurtle" $
       do enableLogging (Just Warn)
          (p1, p2) <- liftIO . tee =<< subscribe "/turtle1/pose"
          (gs1, gs2) <- liftIO . tee . interruptible $ getTraj
          let goals = gate gs2 (cons () (arrivalTrigger gs1 p1))
          advertise "/turtle1/command_velocity" $ 
                    fmap navigate (everyNew goals p2)
