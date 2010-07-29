-- |Demonstration of working with raw ROS images.
module Main (main) where
import Control.Applicative
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import Ros.Sensor_msgs.Image
import Ros.Node
import Ros.StreamCombinators (paired)

-- Convert a 32-bit signed integer to an 8-bit unsigned integer.
toAbsWord :: Int -> Word8
toAbsWord = fromIntegral . abs

-- Convert an 8-bit unsigned integer to a signed 32-bit integer.
toInt :: Word8 -> Int
toInt = fromIntegral

-- Binarize integer values using a threshold.
threshold x | x > 20 = 255
            | otherwise = 0

-- Function that computes the difference from one image to another.
diffImage (i1,i2) = i2 { _data = mask }
    where mask = toBinary . toWords $ diff (toInts pixels1) (toInts pixels2)
          pixels1 = _data i1
          pixels2 = _data i2
          toWords = V.map toAbsWord
          toInts = V.map toInt
          diff = V.zipWith (-)
          toBinary = V.map threshold

main = runNode "/differ" $
       advertise "/diff"  =<< fmap diffImage . paired <$> subscribe "/cam"
