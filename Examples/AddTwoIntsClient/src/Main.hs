module Main (main) where

import Ros.Service.ServiceTypes
import Ros.Service (callService)
import qualified Ros.Rospy_tutorials.AddTwoIntsRequest as Req
import Ros.Rospy_tutorials.AddTwoIntsResponse as Res

type Response a = IO (Either ServiceResponseExcept a)

main :: IO ()
main = do
  response <- callService "/add_two_ints" Req.AddTwoIntsRequest { Req._a=10, Req._b=5 }
  case response of
    Right result -> print (result :: Res.AddTwoIntsResponse)
    Left  e      -> error (show e)
