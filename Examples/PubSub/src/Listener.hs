module Listener (main) where
import Ros.Node
import qualified Ros.Std_msgs.String as S

callback :: S.String -> IO ()
callback = putStrLn . ("I heard " ++) . S._data

main = runNode "listener" $ runHandler callback =<< subscribe "chatter"
