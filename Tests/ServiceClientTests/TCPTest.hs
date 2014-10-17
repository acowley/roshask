--import qualified Ros.Msg.Int64 as I
import Ros.Test_srvs.AddTwoIntsRequest
import Ros.Test_srvs.AddTwoIntsResponse
import Ros.Node.RosTcp (callService)
import Ros.Internal.RosBinary


main :: IO ()
main = serviceStuff

--serviceStuff :: (RosBinary b) => IO (Maybe b)
--serviceStuff = callService rosMast "/add_two_ints" (I.Int64 0)
serviceStuff = do res <- (callService rosMast "/add_two_ints" (AddTwoIntsRequest{a= 42, b= 9})) :: IO (Maybe AddTwoIntsResponse)
                  case res of Nothing -> putStrLn "No result"
                              Just item -> print res
  where
    rosMast = "http://localhost:11311/"
