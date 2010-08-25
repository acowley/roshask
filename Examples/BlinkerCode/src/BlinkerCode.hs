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
import BlobExtraction

findMultiBlobs = undefined
findBlinks = fmap (thresholdBinary 32 1) . finiteDifference absDiff
shiftCode curr prev = cvScaleAdd (cvAndS 127 prev) 2 curr
findLEDs = findMultiBlobs

evensOdds :: Stream a -> (Stream a, Stream a)
evensOdds s = (everyOther s, everyOther (S.tail s))
    where everyOther (Cons x xs) = Cons x (S.tail xs)

both f (x,y) = (f x, f y)
imgToCV img = fromPixels (width img) (height img) (_data img)

main = runNode "BlinkerCodes" $
       do (evens,odds) <- both findBlinks . evensOdds . fmap imgToCV <$> 
                          subscribe "camera"
          let blackImage = fromGrayPixels 640 480 (V.replicate (640*480) 0) 
              codes1 = inStep shiftCode evens (Cons blackImage evens)
              codes2 = inStep shiftCode odds (Cons blackImage odds)
          leds <- liftIO $ merge (fmap findLEDs codes1) (fmap findLEDs codes2)
          advertise "blinkers" leds
