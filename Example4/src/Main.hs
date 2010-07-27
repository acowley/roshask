-- |Demonstration of working with raw ROS images.
module Main (main) where
import Control.Applicative
import qualified Data.Stream as S
import qualified Data.Vector.Storable as V
import qualified Ros.Roslib.Header as H
import Ros.Sensor_msgs.Image
import Ros.Node

-- Turn a stream of values into a stream of pairs of consecutive
-- values.
paired :: Stream a -> Stream (a,a)
paired s = (,) <$> s <*> S.tail s

transformImage img = img { _data = V.map (*2) (_data img) }

-- Function that computes the difference from one image to another.
diffImage (i1,i2) = i2 { _data = V.zipWith (-) (_data i2) (_data i1) }

main = runNode "/haskimg" $
       advertise "/diff"  =<< S.map diffImage . paired <$> subscribe "/images"
       --advertise "/diff"  =<< S.map transformImage <$> subscribe "/images"
