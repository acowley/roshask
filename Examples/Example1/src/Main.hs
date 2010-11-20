module Main (main) where
import Control.Concurrent (threadDelay)
import qualified Ros.Std_msgs.String as S
import Ros.Node

wait n = threadDelay (1000000 * n)

publish = go 1
  where go n = Topic $ wait 3 >> 
                       return (S.String (show n ++ " HA HA HA"), go (n+1))

handle x = putStrLn $ "roskell got " ++ S._data x

main = runNode "/roskell" $ do advertise "/MyMessage" publish
                               chat <- subscribe "/chat"
                               runHandler handle chat
