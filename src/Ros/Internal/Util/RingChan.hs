module Ros.Internal.Util.RingChan (RingChan, newRingChan, writeChan, 
                                   readChan, getChanContents, 
                                   getBuffered) where
import Control.Monad (join)
import Control.Concurrent.MVar
import Control.Concurrent.SSem (SSem)
import qualified Control.Concurrent.SSem as Sem
import qualified Data.Foldable as F
import Data.Sequence (Seq, (|>), viewl, ViewL(..))
import qualified Data.Sequence as Seq
import System.IO.Unsafe (unsafeInterleaveIO)

-- import Control.Concurrent.BoundedChan hiding writeChan

-- type RingChan = BoundedChan
-- newRingChan = newBoundedChan


-- |A 'RingChan' is an 'MVar' containing a triple of maximum capacity, a
-- semaphore used to indicate that the chan has gone from empty to
-- non-empty, and a sequence of items.
type RingChan a = (Int, SSem, MVar (Seq a))

-- |Create a 'RingChan' with the specified maximum capacity.
newRingChan :: Int -> IO (RingChan a)
newRingChan n = do sem <- Sem.new 0
                   q <- newMVar Seq.empty
                   return (n,sem,q)

-- |If the chan is full, does nothing. NOTE: An alternative would be
-- to drop the oldest element before adding the new one.
writeChan :: RingChan a -> a -> IO ()
writeChan (n,sem,mv) x = 
    join $ modifyMVar mv (\q -> if Seq.length q < n
                                then return (q |> x, Sem.signal sem)
                                else let _ :< t = viewl q
                                     in return (t |> x, return ()))
                                -- else return (q, return ()))

-- |Read an item from the channel. Blocks until an item is available.
readChan :: RingChan a -> IO a
readChan (_,sem,mv) = do Sem.wait sem
                         modifyMVar mv (\q -> let h :< t = viewl q
                                              in return (t,h))

getBuffered :: RingChan a -> IO [a]
getBuffered (_,_,xs) = F.toList `fmap` readMVar xs

-- |Return the channel's contents as a lazily realized list.
getChanContents :: RingChan a -> IO [a]
getChanContents c = unsafeInterleaveIO $ do
                      x <- readChan c
                      xs <- getChanContents c
                      return (x:xs)
