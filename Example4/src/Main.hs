-- |Demonstration of working with raw ROS images.
module Main (main) where
import Control.Applicative
import qualified Data.Stream as S
import qualified Data.Vector.Storable as V
import Ros.Sensor_msgs.Image
import Ros.Node

-- Turn a stream of values into a stream of pairs of consecutive
-- values.
paired :: Stream a -> Stream (a,a)
paired s = (,) <$> s <*> S.tail s

-- Absolute difference between two integral numbers. If the numbers
-- are unsigned, then we sign extend them before doing the
-- subtraction, then convert back to the original type.
absDiff :: Integral a => a -> a -> a
absDiff x y = fromIntegral $ abs (x' - y')
    where x' = fromIntegral x :: Int
          y' = fromIntegral y :: Int

-- Function that computes the difference from one image to another.
diffImage (i1,i2) = i2 { _data = mask }
    where mask = V.map threshold $ V.zipWith absDiff (_data i2) (_data i1)
          threshold x | x > 20 = 255
                      | otherwise = 0

main = runNode "/differ" $
       advertise "/diff"  =<< fmap diffImage . paired <$> subscribe "/cam"

