module Listener (main) where
import Ros.Node
import qualified Ros.Std_msgs.String as S

showMsg :: S.String -> IO ()
showMsg = putStrLn . ("I heard " ++) . S._data

main = runNode "listener" $ runHandler showMsg =<< subscribe "chatter"
