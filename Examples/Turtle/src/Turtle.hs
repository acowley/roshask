module Turtle (main) where
import Data.VectorSpace
import Ros.Node
import Ros.Topic (cons, uncons, unfold)
import Ros.TopicUtil (tee, filterBy, everyNew, interruptible)
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

-- Navigate to a goal given a current pose estimate.
navigate :: (Point, Pose) -> Velocity
navigate (goal, pos) = Velocity 2 angVel
  where (vx, vy)     = goal ^-^ (x pos, y pos)
        thetaDesired = atan2 vy vx * (180 / pi)
        thetaErr     = thetaDesired - theta pos
        angVel       = signum thetaErr * (min 2 (abs thetaErr))

posePrinter = runNode "HaskellBTurtle" $ 
              runHandler showPose =<< subscribe "/turtle1/pose"

-- Just move forward
trans = runNode "Go" $ advertise "/turtle1/command_velocity" go
  where go = Topic (threadDelay 1000000 >> return (Velocity 2 0, go))

main = runNode "HaskellBTurtle" $
       do (p1, p2) <- liftIO . tee =<< subscribe "/turtle1/pose"
          (g1, gs) <- liftIO $ uncons (interruptible getTraj)
          let arrived g p = magnitudeSq (g ^-^ p) < 1.5
              p2v p = (x p, y p)
              arrivals = cons g1 (filterBy (fmap arrived (cons g1 gs)) (fmap p2v p1))
          advertise "/turtle1/command_velocity" $ fmap navigate (everyNew arrivals p2)
