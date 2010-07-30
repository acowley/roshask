module Main (main) where
import Control.Applicative
import qualified Data.Vector.Storable as V
import qualified Data.Stream as S
import Data.Word
import Ros.Node
import Ros.Roslib.Header
import Ros.Sensor_msgs.Image
import Ros.StreamCombinators

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

diffImage :: IntImage -> IntImage -> Image
diffImage (IntImage _ _ i1) (IntImage w h i2) = 
    intToImage $ IntImage w h (V.map abs (V.zipWith (-) i1 i2))

-- Show only the parts of the first image that are different from the
-- second.
maskMotion :: IntImage -> IntImage -> Image
maskMotion (IntImage _ _ i1) (IntImage w h i2) = 
    intToImage $ IntImage w h (V.zipWith mask diffs i1)
    where mask diff pix = if diff > 10 then pix else 0
          diffs = V.map abs $ V.zipWith (-) i1 i2

main = runNode "/backsub" $ do
       raw <- fmap toIntPixels <$> subscribe "/cam"
       let avg = weightedMean 0.5 add scale raw
           streamMotion = fmap (uncurry maskMotion)
       advertise "/motion" $ streamMotion (lockstep (S.drop 1 raw) avg)
