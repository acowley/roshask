-- |Demonstration of working with raw ROS images.
module Main (main) where
import Control.Applicative
import qualified Data.Stream as S
import qualified Data.Vector.Storable as V
import Ros.Sensor_msgs.Image
import Ros.Node

import Data.Word

-- Turn a stream of values into a stream of pairs of consecutive
-- values.
paired :: Stream a -> Stream (a,a)
paired s = (,) <$> s <*> S.tail s

transformImage img = img { _data = V.map (*2) (_data img) }

absDiff :: Integral a => a -> a -> a
absDiff x y = fromIntegral $ abs (x' - y')
    where x' = fromIntegral x :: Int
          y' = fromIntegral y :: Int

-- Function that computes the difference from one image to another.
diffImage (i1,i2) = i2 { _data = mask }
    where mask = V.map threshold $ V.zipWith absDiff (_data i2) (_data i1)
          threshold x | x > 20 = 255::Word8
                      | otherwise = 0::Word8

twoFace (i1,i2) = i2 { _data = merged }
    where merged = (V.++) (V.take (V.length (_data i2) `div` 2) (_data i2))
                          (V.drop (V.length (_data i1) `div` 2) (_data i1))

main = runNode "/differ" $
       --advertise "/diff"  =<< fmap twoFace . paired <$> subscribe "/cam"
       advertise "/diff"  =<< fmap diffImage . paired <$> subscribe "/cam"
       --advertise "/diff"  =<< fmap transformImage <$> subscribe "/cam"
