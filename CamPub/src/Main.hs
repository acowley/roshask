module Main (main) where
import Control.Applicative ((<$>))
import Ros.Node
import Ros.Roslib.Header
import Ros.Sensor_msgs.Image (Image(Image))
import AI.CV.OpenCV.HighGui (cvCreateCameraCapture, cvQueryFrame)
import AI.CV.OpenCV.HIplImage (width, height, fromPtr, pixels)
import AI.CV.OpenCV.PixelUtils (toMono)

pubMonoStream = do putStrLn "Starting camera"
                   capture <- cvCreateCameraCapture (-1)
                   let grab = do frame <- cvQueryFrame capture >>= fromPtr
                                 let w = fromIntegral $ width frame
                                     h = fromIntegral $ height frame
                                     head = Header 0 (0,0) ""
                                     pix = toMono frame
                                 return $ Image head h w "mono8" 0 w pix
                       grabStream = Cons grab grabStream
                   return grabStream

-- Start the camera and return a stream of frame grabs.
pubColorStream = do putStrLn "Starting camera"
                    capture <- cvCreateCameraCapture (-1)
                    let grab = do frame <- cvQueryFrame capture >>= fromPtr
                                  let w = fromIntegral $ width frame
                                      h = fromIntegral $ height frame
                                      head = Header 0 (0,0) ""
                                      pix = pixels frame
                                  return $ Image head h w "bgr8" 0 (w*4) pix
                        grabStream = Cons grab grabStream
                    return grabStream

main = do imageCap <- pubMonoStream --pubColorStream
          runNode "/CamPub" (advertiseIO "/cam" imageCap)

