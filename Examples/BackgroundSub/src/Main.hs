module Main (main) where
import Control.Applicative
import qualified Data.Vector.Storable as V
import Data.Word
import Ros.Node
import Ros.Roslib.Header
import Ros.Sensor_msgs.Image
import qualified Ros.TopicUtil as T
import qualified AI.CV.OpenCV.HighCV as H

-- An IntImage has a width, a height, and some integer pixel data.
data IntImage = IntImage Word32 Word32 !(V.Vector Int)

toIntPixels :: Image -> IntImage
toIntPixels img = IntImage (width img) (height img) $ 
                  V.map fromIntegral (_data img)

intToImage :: IntImage -> Image
intToImage (IntImage w h p) = Image header h w "mono8" 0 w $
                              V.map fromIntegral p
    where header = Header 0 (0,0) ""

add :: IntImage -> IntImage -> IntImage
add (IntImage _ _ i1) (IntImage w h i2) = IntImage w h $ V.zipWith (+) i1 i2

iscale c (IntImage w h pix) = IntImage w h $ V.map (*c) pix
shift x (IntImage w h pix) = IntImage w h (V.map (`quot` x) pix)

despeckle w h = H.pixels . H.dilate 8 . H.erode 8 . H.fromGrayPixels w h

maskMotion :: IntImage -> IntImage -> Image
maskMotion (IntImage _ _ i1) (IntImage w h i2) = 
    intToImage $ IntImage w h (V.zipWith applyMask mask i1)
    where mask = despeckle w h $ V.zipWith threshold diffs i2
          applyMask m pix = if m > 0 then pix else 0
          diffs = V.map abs $ V.zipWith (-) i1 i2
          threshold :: Int -> Int -> Word8
          threshold diff avg = if diff > max 1 (avg `quot` 32) then 255 else 0

main = runNode "backsub" $ do
       raw <- fmap toIntPixels <$> subscribe "cam"
       (r1,r2) <- liftIO $ T.tee raw
       let avg = T.weightedMeanNormalized 7 1 add iscale (shift 8) r1
       tname <- getParam' "~topic" "motion"
       advertise tname $ maskMotion <$> T.drop 1 r2 <*> avg
