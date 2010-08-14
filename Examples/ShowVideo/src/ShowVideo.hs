module ShowVideo (main) where
import Control.Applicative
import Control.Concurrent
import Control.Monad (forever, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified MySDL as SDL
import SurfaceUtil
import Ros.Node
import Ros.Sensor_msgs.Image (Image, width, height, _data, encoding)
import Data.Foldable (traverse_)

setVideo :: Int -> Int -> IO SDL.Surface
setVideo w h = SDL.setVideoMode w h 32 [SDL.HWSurface]

sdlInit :: IO (Image -> IO ())
sdlInit = do _ <- SDL.init
             screen <- newIORef =<< setVideo 640 480
             res <- newIORef (640,480)
             frameCount <- newIORef 0
             let showImage img = 
                     do let w = fromIntegral $ width img
                            h = fromIntegral $ height img
                            p = _data img
                        n <- readIORef frameCount
                        writeIORef frameCount $! n+1
                        putStrLn $ "Frame "++show n
                        (w',h') <- readIORef res
                        when (w /= w' || h /= h')
                             (do writeIORef res (w,h)
                                 setVideo w h >>= writeIORef screen)
                        screen' <- readIORef screen
                        image <- if encoding img == "mono8"
                                 then createGraySurfaceFrom [SDL.HWSurface] w h p
                                 else createRGBSurfaceFrom [SDL.HWSurface] w h p
                        SDL.blitSurface image Nothing screen' Nothing
                        SDL.flip screen'
                        _ <- SDL.pollEvent
                        return ()
             return showImage

myTraverse :: (Image -> IO ()) -> Stream a -> IO ()
myTraverse f (Cons x xs) = (f $! x) >> myTraverse f xs

main = do showImage <- sdlInit
          runNode "ShowVideo" $
            do source <- getParam' "~video" "/video"
               images <- subscribe source
               runHandler $ myTraverse showImage images --(traverse_ showImage images)
