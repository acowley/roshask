module Main (main) where
import Control.Applicative
import qualified Ros.Std_msgs.String as S
import Ros.Std_msgs.Int32 (Int32(..))
import Ros.Node

lengthTransform msg = Int32 (length (S._data msg))

main = runNode "/roskell" $ 
       advertise "/len" =<< fmap lengthTransform <$> subscribe "/chat"
