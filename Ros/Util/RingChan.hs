module Ros.Util.RingChan (RingChan, newRingChan, writeChan, 
                          readChan, getChanContents) where
import Control.Applicative
import Control.Monad (join)
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Monad (when)
import Data.Sequence (Seq, (|>), viewl, ViewL(..))
import qualified Data.Sequence as Seq
import System.IO.Unsafe

-- import Control.Concurrent.BoundedChan hiding writeChan

-- type RingChan = BoundedChan
-- newRingChan = newBoundedChan


-- |A 'RingChan' is an 'MVar' containing a triple of maximum capacity, a
-- semaphore used to indicate that the chan has gone from empty to
-- non-empty, and a sequence of items.
type RingChan a = (Int, QSem, MVar (Seq a))

-- |Create a 'RingChan' with the specified maximum capacity.
newRingChan :: Int -> IO (RingChan a)
newRingChan n = do sem <- newQSem 0
                   q <- newMVar Seq.empty
                   return (n,sem,q)

-- |If the chan is full, drop the oldest element. NOTE: Or just don't
-- add the new element?
writeChan :: RingChan a -> a -> IO ()
writeChan (n,sem,mv) x = 
    join $ modifyMVar mv (\q -> if Seq.length q < n
                                then return (q |> x, signalQSem sem)
--                                 else let _ :< t = viewl q
--                                      in return (t |> x, return ()))
                                else return (q, return ()))

-- |Read an item from the channel. Blocks until an item is available.
readChan :: RingChan a -> IO a
readChan (_,sem,mv) = do waitQSem sem
                         modifyMVar mv (\q -> let h :< t = viewl q
                                              in return (t,h))

-- |Return the channel's contents as a lazily realized list.
getChanContents :: RingChan a -> IO [a]
getChanContents c = unsafeInterleaveIO $ do
                      x <- readChan c
                      xs <- getChanContents c
                      return (x:xs)
