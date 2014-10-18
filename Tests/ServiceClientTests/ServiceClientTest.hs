module Main where
import Ros.Test_srvs.AddTwoIntsRequest
import Ros.Test_srvs.AddTwoIntsResponse
import Ros.Service (callService)
import Test.Tasty
import Test.Tasty.HUnit
import GHC.Int

-- To run:
-- 1. start ros: run "roscore"
-- 2. in another terminal start the add_two_ints server:
--      python roshask/Tests/ServiceClientTests/add_two_ints_server.py
-- 3. in a new terminal make sure $ROS_MASTER_URI is correct and run
--      cabal test servicetest --show-details=always

main :: IO ()
main = defaultMain $ testGroup "Service Tests" [addIntsTest 4 7]

addIntsTest :: GHC.Int.Int64 -> GHC.Int.Int64 -> TestTree
addIntsTest x y = testCase ("add_two_ints, add " ++ show x ++ " + " ++ show y) $
  do res <- callService "/add_two_ints" AddTwoIntsRequest{a=x, b=y} :: IO (Maybe AddTwoIntsResponse)
     Just (AddTwoIntsResponse (x + y)) @?= res
