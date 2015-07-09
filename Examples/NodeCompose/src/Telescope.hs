-- |A node that advertises a video feed from a telescope sensor for
-- tracking UFOs.
module Telescope (telescope, main) where
import Ros.Node
import Ros.Topic (repeatM)
import Ros.Topic.Transformers (runTopicState')
import Ros.Std_msgs.Header (Header(..))
import Ros.Sensor_msgs.Image (Image(..))
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Control.Monad.State.Strict

-- Draw a white point orbiting the origin on a black background. The
-- state of the point is the angle in degrees it makes with the
-- origin.
orbitPt :: Monad m => Int -> Int -> StateT Int m (V.Vector Word8)
orbitPt w h = do theta <- get
                 let x = hw + (floor $ r * cos (fromIntegral theta * pi / 180))
                     y = hh + (floor $ r * sin (fromIntegral theta * pi / 180))
                     p = y * w + x
                     img = V.create $ do v <- V.thaw background
                                         VM.write v p 255
                                         return v
                 modify ((`rem` 360) . (+1))
                 return img
  where sz = w * h
        r = fromIntegral $ w `div` 4
        hw = w `div` 2
        hh = h `div` 2
        background = V.create $ VM.replicate sz 0

-- Generate a stream of images of an orbiting point.
images :: Topic (StateT Int IO) Image
images = repeatM (mkImage `fmap` orbitPt width height)
  where width = 1024
        height = 1024
        header = Header 0 (0,0) ""
        fi = fromIntegral
        mkImage = Image header (fi width) (fi height) "mono8" 0 (fi width)

telescope :: Node ()
telescope = advertise "video" $ (topicRate 60 (runTopicState' images 0))

main = runNode "Scope" telescope
