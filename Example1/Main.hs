module Main (main) where
import Control.Concurrent (threadDelay)
import qualified Ros.Std_msgs.String as S
import Ros.Node
import Ros.RosTypes

genMsg = do threadDelay 3000000
            return s
    where s = S.String "hi, guy"

publish = Stream genMsg publish

handle (Stream m ms) = putStrLn ("roskell got "++S._data m) >> handle ms

main = runNode "/roskell" $ do advertiseIO "/MyMessage" publish
                               chat <- subscribe "/chat"
                               runHandler (handle chat)
             --runNode "/roskell" (advertiseIO "/MyMessage" publish)