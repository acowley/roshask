module Listener (main) where
import Ros.Node
import Lens.Family (view)
import qualified Ros.Std_msgs.String as S

showMsg :: S.String -> IO ()
showMsg = putStrLn . ("I heard " ++) . view S._data

main = runNode "listener" $ runHandler showMsg =<< subscribe "chatter"
