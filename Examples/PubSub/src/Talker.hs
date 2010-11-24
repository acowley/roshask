module Talker (main) where
import Data.Time.Clock (getCurrentTime)
import Ros.Node
import Ros.Topic (unfold)
import qualified Ros.Std_msgs.String as S

sayHello :: IO (Topic IO S.String)
sayHello = toTopic `fmap` rateLimiter 1 getCurrentTime
  where toTopic = fmap (S.String . ("Hello world " ++) . show) . unfold

main = runNode "talker" $ advertise "chatter" =<< liftIO sayHello
