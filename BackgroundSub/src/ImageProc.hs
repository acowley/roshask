-- |High level wrappers for OpenCV erosion and dilation morphological
-- operators.
module ImageProc where
import AI.CV.OpenCV.HIplImage
import qualified AI.CV.OpenCV.HighCV as H
import Data.Word (Word8)
import qualified Data.Vector.Storable as V

erode :: Integral a => a -> a -> Int -> V.Vector Word8 -> V.Vector Word8
erode w h n pix = pixels $ H.erode n (fromPixels w h pix)

dilate :: Integral a => a -> a -> Int -> V.Vector Word8 -> V.Vector Word8
dilate w h n pix = pixels $ H.dilate n (fromPixels w h pix)
