module Main (main) where
import Ros.Node
import Ros.Roslib.Header
import Ros.Sensor_msgs.Image (Image(Image))
import AI.CV.OpenCV.HighGui (cvCreateCameraCapture, cvQueryFrame)
import AI.CV.OpenCV.HIplImage (width, height, fromPtr)
import AI.CV.OpenCV.PixelUtils (toMono)

-- Start the camera and return a stream of frame grabs.
pubImageStream = do putStrLn "Starting camera"
                    capture <- cvCreateCameraCapture (-1)
                    let grab = do frame <- cvQueryFrame capture >>= fromPtr
                                  let w = fromIntegral $ width frame
                                      h = fromIntegral $ height frame
                                      pix = toMono frame
                                      head = Header 0 (0,0) ""
                                  return $ Image head h w "mono8" 0 w pix
                        grabStream = Cons grab grabStream
                    return grabStream

main = do imageCap <- pubImageStream 
          runNode "/CamPub" (advertiseIO "/cam" imageCap)

