module Main (main) where
import Ros.Node
import Ros.Topic (unfold)
import Ros.TopicUtil (tee)
import Ros.Rate
import Ros.Roslib.Header
import Ros.Sensor_msgs.Image (Image(Image))
import AI.CV.OpenCV.HighCV (width, height, pixels, isColor, createCameraCapture)
import AI.CV.OpenCV.PixelUtils (toMono, packPixels)

-- Start the camera and return a stream of frame grabs.
grabStream rate = do putStrLn "Starting camera"
                     capture <- createCameraCapture Nothing
                     grab <- rateLimiter rate $ 
                             do frame <- capture
                                let w = fromIntegral $ width frame
                                    h = fromIntegral $ height frame
                                    head = Header 0 (0,0) ""
                                return (w,h,head, isColor frame)
                     return $ unfold grab

pubMono (w,h,head,frame) = Image head h w "mono8" 0 w (toMono frame)
pubColor (w,h,head,frame) = Image head h w "bgr8" 0 (w*3) (packPixels frame)

main = runNode "/CamPub" $ 
       do rateCap <- getParam' "~rate" 30
          (img1,img2) <- liftIO $ tee =<< grabStream rateCap
          advertise "/cam" $ fmap pubMono img1
          advertise "/cam_color" $ fmap pubColor img2
                              

