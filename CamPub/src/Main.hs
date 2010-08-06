module Main (main) where
import Ros.Node
import Ros.Roslib.Header
import Ros.Sensor_msgs.Image (Image(Image))
import AI.CV.OpenCV.HighGui (cvCreateCameraCapture, cvQueryFrame)
import AI.CV.OpenCV.HighCV (width, height, fromPtr, pixels, isColor)
import AI.CV.OpenCV.PixelUtils (toMono, packPixels)

-- Start the camera and return a stream of frame grabs.
grabStream = do putStrLn "Starting camera"
                capture <- cvCreateCameraCapture (-1)
                let grab = do frame <- cvQueryFrame capture >>= fromPtr
                              let w = fromIntegral $ width frame
                                  h = fromIntegral $ height frame
                                  head = Header 0 (0,0) ""
                              return (w,h,head, isColor frame)
                    grabAll = Cons grab grabAll
                return grabAll

pubMono (w,h,head,frame) = Image head h w "mono8" 0 w (toMono frame)
--pubColor (w,h,head,frame) = Image head h w "bgr8" 0 (w*4) (pixels frame)
pubColor (w,h,head,frame) = Image head h w "bgr8" 0 (w*3) (packPixels frame)

main = do images <- grabStream
          let monoStream = fmap (fmap pubMono) images
              colorStream = fmap (fmap pubColor) images
          runNode "/CamPub" $ do advertiseIO "/cam" monoStream
                                 advertiseIO "/cam_color" colorStream
                              

