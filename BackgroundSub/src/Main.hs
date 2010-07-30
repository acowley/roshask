module Main (main) where
import Control.Applicative
import qualified Data.Vector.Storable as V
import qualified Data.Stream as S
import Data.Word
import Ros.Node
import Ros.Roslib.Header
import Ros.Sensor_msgs.Image
import Ros.StreamCombinators
import ImageProc

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

scale :: Float -> IntImage -> IntImage
scale c (IntImage w h pix) = IntImage w h $ V.map (truncate.(*c).fromIntegral) pix

iscale c (IntImage w h pix) = IntImage w h $ V.map (*c) pix
shift x (IntImage w h pix) = IntImage w h (V.map (`div` x) pix)

maskMotion :: IntImage -> IntImage -> Image
maskMotion (IntImage _ _ i1) (IntImage w h i2) = 
    intToImage $ IntImage w h (V.zipWith applyMask mask i1)
    where mask = dilate w h (erode w h (V.zipWith threshold diffs i2) 8) 8
          applyMask m pix = if m > 0 then pix else 0
          diffs = V.map abs $ V.zipWith (-) i1 i2
          threshold diff avg = if diff > max 1 (avg `div` 32) then 255 else 0

main = runNode "/backsub" $ do
       raw <- fmap toIntPixels <$> subscribe "/cam"
       let --avg = weightedMean 0.9 add scale raw
           avg = weightedMeanNormalized 7 1 add iscale (shift 8) raw
           streamMotion = fmap (uncurry maskMotion)
       advertise "/motion" $ streamMotion (lockstep (S.drop 1 raw) avg)
