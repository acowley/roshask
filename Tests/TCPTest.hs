--import qualified Ros.Msg.Int64 as I
import Ros.Test_srvs.AddTwoIntsRequest
import Ros.Node.RosTcp (callService)


main :: IO ()
main = serviceStuff

serviceStuff :: IO b
--serviceStuff = callService rosMast "/add_two_ints" (I.Int64 0)
serviceStuff = callService rosMast "/add_two_ints" (AddTwoIntsRequest{a=0, b=0})
  where
    rosMast = "http://localhost:11311/"
