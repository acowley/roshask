module Main (main) where
import qualified Ros.Rospy_tutorials.AddTwoIntsRequest as Req
import qualified Ros.Rospy_tutorials.AddTwoIntsResponse as Res
import Ros.Service (callService)
import Ros.Service.ServiceTypes

type Response a = IO (Either ServiceResponseExcept a)

main :: IO ()
main = do
  response <- callService "/add_two_ints" Req.AddTwoIntsRequest{Req.a=10, Req.b=5} :: Response  Res.AddTwoIntsResponse
  print response
