--import qualified Ros.Msg.Int64 as I
import Ros.Test_srvs.AddTwoIntsRequest
import Ros.Test_srvs.AddTwoIntsResponse
import Ros.Node.RosTcp (callService)
import Ros.Internal.RosBinary


-- To run:
-- 0. Install roshask
-- 1. start ros, run "roscore"
-- 2. in another terminal start the add_two_ints server:
--      "roscd rospy_tutorials"
--      "python 005_add_two_ints/add_two_ints_server"
--3. in roshask/Tests/ServiceClientTests run
--      "runhaskell ServiceClientTest.hs"
main :: IO ()
main = clientTest

clientTest :: IO ()
clientTest = do res <- (callService rosMast "/add_two_ints" (AddTwoIntsRequest{a= 42, b= 9})) :: IO (Maybe AddTwoIntsResponse)
                case res of Nothing -> putStrLn "No result"
                            Just item -> print res
  where
    rosMast = "http://localhost:11311/"
