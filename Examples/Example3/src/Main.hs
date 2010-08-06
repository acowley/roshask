module Main (main) where
import Control.Applicative
import qualified Ros.Std_msgs.String as S
import Ros.Std_msgs.Int32 (Int32(..))
import Ros.Node

lengthTransform (Cons m ms) = Cons (Int32 (length (S._data m))) 
                                   (lengthTransform ms)

main = runNode "/roskell" $ 
       advertise "/len" =<< lengthTransform <$> subscribe "/chat"
