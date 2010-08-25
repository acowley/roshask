module BlinkerCode (main) where
import Control.Applicative
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import Ros.Node
import qualified Ros.Stream as S
import Ros.StreamCombinators
import AI.CV.OpenCV.HighCV hiding (width, height)
import AI.CV.OpenCV.Threshold
import AI.CV.OpenCV.ArrayOps
import Ros.Sensor_msgs.Image 
import BlobExtraction (findMultiBlobs)

findBlinks = fmap (thresholdBinary 32 1) . finiteDifference absDiff
shiftCode curr prev = cvScaleAdd (cvAndS 127 prev) 2 curr
findBlinkers = findMultiBlobs

imgToCV :: Image -> HIplImage FreshImage MonoChromatic Word8
imgToCV img = fromPixels (width img) (height img) (_data img)

evensOdds :: Stream a -> (Stream a, Stream a)
evensOdds s = (fmap everyOther s, fmap everyOther (S.tail s))
    where everyOther Cons x xs = Cons x (S.tail xs)

main = runNode "BlinkerCodes" $
       do images <- fmap imgToCV <$> subscribe "camera"
          let blackImage = fromGrayPixels 640 480 (V.replicate (640*480) 0) 
              blinks = findBlinks images
              codes = inStep shiftCode blinks (Cons blackImage blinks)
          advertise "blinkers" $ fmap findBlinkers codes
