-- |High level wrappers for OpenCV erosion and dilation morphological
-- operators.
module ImageProc where
import AI.CV.OpenCV.HIplImage hiding (erode, dilate)
import qualified AI.CV.OpenCV.HIplImage as H
import Data.Word (Word8)
import qualified Data.Vector.Storable as V

erode :: Integral a => a -> a -> V.Vector Word8 -> Int -> V.Vector Word8
erode w h pix n = pixels $ H.erode (fromPixels w h pix) n

dilate :: Integral a => a -> a -> V.Vector Word8 -> Int -> V.Vector Word8
dilate w h pix n = pixels $ H.dilate (fromPixels w h pix) n
