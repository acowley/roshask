module Main (main) where
import Control.Concurrent
import qualified Ros.Std_msgs.String as S
import Ros.Node
import Ros.RosTypes
import Ros.RosBinary
import System.IO.Unsafe

genMsg = do threadDelay 5000000
            return s
    where s = S.String "hi, guy"

publish = Stream genMsg publish

main = let s = S.String "'erro"
       in do putStrLn (S._data s)
             runNode "ROSKELL" (advertiseIO "MyMessage" publish)