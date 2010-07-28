module Main (main) where
import Control.Concurrent (forkIO)
import Control.Concurrent.BoundedChan
import Control.Monad (forever)
import Ros.Node
import Ros.Roslib.Header
import Ros.Sensor_msgs.Image (Image(Image))
import AI.CV.OpenCV.HighGui (cvCreateCameraCapture, cvQueryFrame)
import AI.CV.OpenCV.HIplImage (width, height, fromPtr)
import AI.CV.OpenCV.PixelUtils (toMono)

-- NOTE: I think the problem here is that cvCreateCameraCapture is
-- putting something in thread local storage. That means that I need
-- to run the camera capture in its own thread.

pubImages chan = do putStrLn "Starting camera"
                    capture <- cvCreateCameraCapture (-1)
                    let grab = do frame <- cvQueryFrame capture >>= fromPtr
                                  let w = fromIntegral $ width frame
                                      h = fromIntegral $ height frame
                                      pix = toMono frame
                                      head = Header 0 (0,0) ""
                                  return $ Image head h w "mono8" 0 w pix
                    forever (grab >>= writeChan chan)

main = do chan <- newBoundedChan 5
          t <- forkIO $ pubImages chan
          let passAlong = Cons (readChan chan) passAlong
          runNode "/CamPub" (advertiseIO "/cam" passAlong)
          --release_capture capture
