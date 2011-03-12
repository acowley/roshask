{-# LANGUAGE NamedFieldPuns #-}
-- |A Node that detects UFOs (white pixels on a black sky background)
-- in a video feed from a sensor.
module DetectUFO (detectUFO, main) where
import Ros.Node
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Ros.Sensor_msgs.Image (Image(..), width, height, encoding, _data)

findPt :: Image -> IO ()
findPt (Image {width, height, encoding, _data})
  | encoding == "mono8" = maybe noPt showPt p 
  | otherwise = putStrLn "Unsupported image format"
  where p = V.elemIndex 255 _data
        toTheta = iatan2 . translate . (`divMod` fi width)
        iatan2 (y,x) = atan2 (fromIntegral y) (fromIntegral x) * 180 / pi
        showPt index = putStrLn $ "UFO at angle: " ++ show (toTheta index)
        noPt = putStrLn "Couldn't find UFO"
        fi = fromIntegral
        translate (y,x) = (y - (fi height `div` 2), x - (fi width `div` 2))

detectUFO :: Node ()
detectUFO = subscribe "video" >>= runHandler findPt >> return ()

main = runNode "Detect" detectUFO
