module Ros.Publisher where
import Ros.RosTypes

newtype Publisher a = Publisher { unPublish :: Stream a -> IO (Stream a }

instance Monad Publisher where
    return x = Publisher (\xs -> Stream x xs)
    a >>= b = Publisher $ do b' <- b
                             unPublish a b

publish :: a -> Publisher a
publish = Publisher . Stream
