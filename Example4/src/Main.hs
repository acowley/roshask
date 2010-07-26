-- |Demonstration of working with raw ROS images.
module Main (main) where
import Control.Applicative
import qualified Data.Vector.Storable as V
import qualified Ros.Roslib.Header as H
import Ros.Sensor_msgs.Image
import Ros.Node

-- Turn a stream of values into a stream of pairs of consecutive
-- values.
paired :: Stream a -> Stream (a,a)
paired (Stream x xs) = go x xs
    where go prev (Stream x xs) = Stream (prev,x) (go x xs)

transformImage (Stream img imgs) = 
    Stream (img {_data = V.map (*2) (_data img)}) (transformImage imgs)

main = runNode "/haskimg" $
       advertise "/diff"  =<< transformImage <$> subscribe "/images"
