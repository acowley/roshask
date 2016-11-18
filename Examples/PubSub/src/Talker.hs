module Talker (main) where
import Data.Time.Clock (getCurrentTime)
import Ros.Node
import Ros.Topic (repeatM)
import Ros.Topic.Util (topicRate)
import qualified Ros.Std_msgs.String as S

sayHello :: Topic IO S.String
sayHello = repeatM (fmap mkMsg getCurrentTime)
  where mkMsg = S.String . ("Hello world " ++) . show

main = runNode "talker" $ advertise "chatter" (topicRate 1 sayHello)
