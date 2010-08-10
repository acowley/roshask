{-# LANGUAGE PackageImports #-}
module TrajFollower (main) where
import Control.Applicative
import "monads-fd" Control.Monad.State (liftIO)
import Ros.Node
import Ros.Geometry_msgs.Point (Point(..))
import Ros.Geometry_msgs.Pose (position)
import Ros.Geometry_msgs.PoseStamped (PoseStamped, pose)
import Ros.Nav_msgs.Path (Path, poses)
import qualified Ros.Stream as S
import Ros.StreamCombinators (everyNew, filterBy, gate, concats)

(<->) :: Point -> Point -> Point
Point x1 y1 z1 <-> Point x2 y2 z2 = Point (x1-x2) (y1-y2) (z1-z2)

scale :: Double -> Point -> Point
scale s (Point x y z) = Point (s*x) (s*y) (s*z)

norm :: Point -> Double
norm (Point x y z) = sqrt $ x*x + y*y + z*z

arrived :: PoseStamped -> PoseStamped -> Bool
arrived goal pos = norm (pp goal <-> pp pos) < threshold
    where pp = position . pose
          threshold = 1

goalStream :: Stream Path -> Stream PoseStamped -> Stream PoseStamped
goalStream plans pos = Cons g $ gate goals arrivals
    where gs@(Cons g goals) = concats $ fmap poses plans
          arrivals = filterBy (fmap arrived gs) pos

move :: Stream (PoseStamped, PoseStamped) -> IO ()
move (Cons (goal,curPos) xs) = 
    putStrLn ("Moving from "++show curPos++" to "++show goal) >> move xs

main = runNode "traj" $ 
       do plans <- subscribe "/plan"
          currentPos <- subscribe "/pos"
          let goals = goalStream plans currentPos
          followerState <- liftIO $ streamIO =<< everyNew goals currentPos
          runHandler $ move followerState
