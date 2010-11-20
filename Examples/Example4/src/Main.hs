-- |Demonstration of working with raw ROS images.
module Main (main) where
import Control.Applicative
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import Ros.Sensor_msgs.Image
import Ros.Node
import Ros.TopicUtil (finiteDifference)

-- Convert a 32-bit signed integer to an 8-bit unsigned integer.
toAbsWords :: V.Vector Int -> V.Vector Word8
toAbsWords = V.map (fromIntegral . abs)

-- Convert an 8-bit unsigned integer to a signed 32-bit integer.
toInts :: V.Vector Word8 -> V.Vector Int
toInts = V.map fromIntegral

-- Binarize integer values using a threshold.
threshold x | x > 20 = 255
            | otherwise = 0

-- Function that computes the difference from one image to another.
diffImage i1 i2 = i2 { _data = mask }
    where mask = toBinary . toAbsWords $ diff ipixels1 ipixels2
          ipixels1 = toInts $ _data i1
          ipixels2 = toInts $ _data i2
          diff = V.zipWith (-)
          toBinary = V.map threshold

main = runNode "/differ" $
       advertise "/diff"  =<< finiteDifference diffImage <$> subscribe "/cam"
