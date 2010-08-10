module FastAndSlow (main) where
import Ros.Node
import Ros.StreamCombinators
import qualified Ros.Stream as S
import Control.Concurrent
import Data.Either (rights)
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

discriminated :: Stream a -> Stream b -> IO (Stream (Either a b))
discriminated xs ys = fmap Left xs <|> fmap Right ys

-- Produce a Stream of the same type as the first but with the same
-- production rate as the second, on average.
interpolate :: (a -> a -> [b] -> [c]) -> Stream a -> Stream b -> IO (Stream c)
interpolate f xs clock = 
    do fused <- discriminated xs clock
       let Cons (Left p1) xs' = S.dropWhile (not . isLeft) fused
       return . concats $ go p1 xs'
    where go p1 xs = let (window, Cons (Left p2) xs') = S.break isLeft xs
                     in Cons (f p1 p2 (rights window)) (go p2 xs')
          isLeft (Left _) = True
          isLeft _ = False

testInterp :: IO (Stream Float)
testInterp = do slow <- streamIO slowNums :: IO (Stream Int)
                fast <- streamIO fastUnits :: IO (Stream ())
                interpolate f slow fast
    where slowNums = let go x = Cons (threadDelay 5000000 >> return x) (go (x+1)) 
                     in go 0
          fastUnits = let go = Cons (threadDelay 500000 >> return ()) go
                      in go
          f :: Int -> Int -> [()] -> [Float]
          f n1 n2 ticks = let n1' = fromIntegral n1
                              n2' = fromIntegral n2
                              step = (n2' - n1') / fromIntegral (length ticks)
                          in [n1', n1' + step .. n2']

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
