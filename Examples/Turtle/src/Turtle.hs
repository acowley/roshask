module Turtle (main) where
import Data.VectorSpace
import Ros.Node
import Ros.Topic (cons, uncons, unfold)
import Ros.TopicUtil (tee, filterBy, everyNew, interruptible)
import Ros.Turtlesim.Pose
import Ros.Turtlesim.Velocity

-- A type synonym for a 2D point.
type Point = (Float,Float)

showPose :: Pose -> IO ()
showPose = putStrLn . show

-- A Topic of user-supplied waypoint trajectories.
getTraj :: Topic IO [Point]
getTraj = unfold (putStr "Enter waypoints: " >> read `fmap` getLine)

-- Navigate to a goal given a current pose estimate.
navigate :: (Point, Pose) -> Velocity
navigate (goal, pos) = Velocity 0.1 angVel
  where (vx, vy)     = goal ^-^ (x pos, y pos)
        thetaDesired = atan2 vy vx * (180 / pi)
        thetaErr     = thetaDesired - theta pos
        angVel       = signum thetaErr * (min 5 thetaErr)

-- main = runNode "HaskellBTurtle" $ 
--        runHandler showPose =<< subscribe "/turtle1/pose"

main = runNode "HaskellBTurtle" $
       do (p1, p2) <- liftIO . tee =<< subscribe "/turtle1/pose"
          (g1, gs) <- liftIO $ uncons (interruptible getTraj)
          let arrived g p = magnitudeSq (g ^-^ p) < 0.2
              p2v p = (x p, y p)
              arrivals = cons g1 (filterBy (fmap arrived gs) (fmap p2v p1))
          advertise "vel" $ fmap navigate (everyNew arrivals p2)
