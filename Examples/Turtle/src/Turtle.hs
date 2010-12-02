module Turtle (main) where
import Data.VectorSpace
import Ros.Node
import Ros.Topic (cons, uncons, unfold)
import Ros.TopicUtil (tee, filterBy, everyNew, interruptible, gate)
import Ros.Turtlesim.Pose
import Ros.Turtlesim.Velocity
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)

-- A type synonym for a 2D point.
type Point = (Float,Float)

showPose :: Pose -> IO ()
showPose = putStrLn . show

-- A Topic of user-supplied waypoint trajectories.
getTraj :: Topic IO [Point]
getTraj = unfold (putStr "Enter waypoints: " >> hFlush stdout >> read `fmap` getLine)

wrapAngle theta | theta < 0 = theta + 2 * pi
wrapAngle theta | theta > 2*pi = theta - 2 * pi
wrapAngle theta = theta

angleDiff x y = let x' = if x < 0 then x + 2*pi else x
                    y' = if y < 0 then y + 2*pi else y
                in wrapAngle (x' + pi - y') - pi

-- Navigate to a goal given a current pose estimate.
navigate :: (Point, Pose) -> Velocity
navigate (goal, pos) = Velocity (min 2 (magnitude v)) angVel
  where v@(vx, vy)   = goal ^-^ (x pos, y pos)
        thetaDesired = atan2 vy vx
        thetaErr     = (angleDiff thetaDesired (theta pos)) * (180 / pi)
        angVel       = signum thetaErr * (min 2 (abs thetaErr))

main = runNode "HaskellBTurtle" $
       do (p1, p2) <- liftIO . tee =<< subscribe "/turtle1/pose"
          (gs1, gs2) <- liftIO . tee . interruptible $ getTraj
          let arrived g p = magnitude (g ^-^ p) < 1.5
              p2v p = (x p, y p)
              arrivals = filterBy (fmap arrived gs1) (fmap p2v p1)
              goals = gate gs2 (cons () (fmap (const ()) arrivals))
          advertise "/turtle1/command_velocity" $ 
                    fmap navigate (everyNew goals p2)
