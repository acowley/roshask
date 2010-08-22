import Control.Concurrent
import Data.Foldable (traverse_)
import Ros.Node
import qualified Ros.Stream as S
import Ros.StreamCombinators

genPaths1 :: (Enum a, Ord a) => a -> a -> Stream (IO [a])
genPaths1 x y = chunks x
    where go start = do threadDelay 1000000
                        return $ take 5 (enumFrom start)
          chunks start = let nxt = head . drop 5 $ iterate succ start
                             nxt' = if nxt > y then x else nxt
                         in Cons (go start) $ chunks nxt'

consumer :: Show a => Stream a -> IO ()
consumer = traverse_ go
    where go x = do putStrLn $ "Consuming "++show x
                    threadDelay 600000

main = do ps <- streamIO (genPaths1 'A' 'Z')
          ps' <- interruptible ps
          consumer ps'
          