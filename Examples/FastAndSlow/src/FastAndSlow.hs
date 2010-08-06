module FastAndSlow (main) where
import Ros.Node
import Ros.StreamCombinators
import Control.Concurrent
import Data.Foldable (traverse_)
import Data.IORef
import System.IO.Unsafe

fastProducer = aux 0
    where go x = threadDelay 1000000 >> return x
          aux x = Cons (go x) (aux (x+1))

slowProducer = aux 'A'
    where go c = threadDelay 5000000 >> return c
          aux 'Z' = Cons (go 'Z') (aux 'A')
          aux c = Cons (go c) (aux (toEnum (fromEnum c + 1)))

consume :: Show a => Stream a -> IO ()
consume = traverse_ (putStrLn . show)

funnel = do fp <- streamIO fastProducer
            sp <- fmap (fmap fromEnum) $ streamIO slowProducer
            interleaved <- fp <|> sp
            consume interleaved

main = do fp <- streamIO fastProducer
          sp <- streamIO slowProducer
          --fused <- bothNew fp sp >>= streamIO
          fused <- everyNew fp sp >>= streamIO
          consume fused

statefulProducer = do r <- newIORef 0
                      let go = do x <- readIORef r
                                  xs <- unsafeInterleaveIO go
                                  threadDelay 1000000
                                  writeIORef r (x+1)
                                  return $ Cons x xs
                      go

evilConsumer s (Cons x xs@(Cons y ys)) = do putStrLn $ s++"y = "++show y
                                            putStrLn $ s++"x = "++show x
                                            evilConsumer s xs

stress = do sp <- statefulProducer
            q <- newQSem 0 
            t1 <- forkIO $ evilConsumer "Uno: " sp
            t2 <- forkIO $ evilConsumer "Dos: " sp
            waitQSem q
            killThread t1
            killThread t2
