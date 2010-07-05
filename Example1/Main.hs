module Main (main) where
import Control.Concurrent
import qualified Ros.Std_msgs.String as S
import Ros.Node
import Ros.RosTypes
import System.IO.Unsafe

sendMsg = let s = S.String "hi, guy"
          in Stream s $
             unsafeInterleaveIO (threadDelay 5000000 >> return sendMsg)

main = let s = S.String "'erro"
       in do putStrLn (S._data s)
             runNode $ "ROSKELL" (advertise "MyMessage" sendMsg)