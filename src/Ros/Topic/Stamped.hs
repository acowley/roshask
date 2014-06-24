{-# LANGUAGE TupleSections #-}
-- |Functions for fusing 'Topic's based on TimeStamp fields of the
-- underlying messages. This module shadows some of the functionality
-- of the "Ros.TopicUtil" module. The difference is that the functions
-- exported by this module use time stamps to correlate two
-- 'Topic's. 
-- 
-- The correlation uses a bracketing pair of values from one 'Topic'
-- to pick a correspondance for each value from the other
-- 'Topic'. This bracketing approach induces some latency. The most
-- common use case is calling the 'bothNew' function with a 'Topic'
-- that produces very quickly (faster than the minimum required update
-- rate), and another 'Topic' that imposes a rate limit.
module Ros.Topic.Stamped (everyNew, interpolate, batch) where
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Timeout
import qualified Ros.Topic as T
import Ros.Topic (Topic(..), metamorphM, yieldM)
import qualified Ros.Topic.Util as T
import Ros.Internal.Msg.HeaderSupport
import Ros.Internal.RosTime

-- |Given two consecutive values, pick the one with the closest time
-- stamp to another value.
pickNearest :: (HasHeader a, HasHeader b) => a -> a -> b -> a
pickNearest x1 x2 y
  | ty <= t1 = x1
  | t2 <= ty = x2
  | d1 < d2 = x1
  | otherwise = x2
  where t1 = getStamp x1
        t2 = getStamp x2
        ty = getStamp y
        d1 = diffROSTime ty t1
        d2 = diffROSTime t2 ty

-- |@findBrackets t1 t2@ Pairs each element of @t2@ with the pair of
-- consecutive elements from @t1@ that brackets it in time, and the 
-- time interval in seconds covered by that bracket.
findBrackets :: (HasHeader a, HasHeader b) =>
                Topic IO a -> Topic IO b -> Topic IO ((a,a,Double), b)
findBrackets t1 t2 = T.concats . metamorphM (go t2) $ T.consecutive t1
  where go t (x,y) = let start = getStamp x
                         stop = getStamp y
                         bracket = (x,y, diffSeconds stop start)
                     in do (items, rest) <- T.break ((< stop) . getStamp) $
                                            T.dropWhile ((< start) . getStamp) t
                           let items' = map (bracket,) items
                           yieldM items' (go rest)

-- |Remove an element from a 'Topic' if the next element from that
-- 'Topic' is composed of elements bearing the exact same sequence
-- IDs in their headers (as obtained by 'getSequence').
removeDups :: (Functor m, Monad m, HasHeader a, HasHeader b) =>
              Topic m (a,b) -> Topic m (a,b)
removeDups = T.catMaybes . fmap check . T.consecutive
  where check ((x1,y1), x@(x2,y2))
          | getSequence x1 == getSequence x2 && 
            getSequence y1 == getSequence y2 = Nothing
          | otherwise = Just x

-- |Returns a 'Topic' that produces a new pair for every value
-- produced by either of the component 'Topic's. The value of the
-- other element of the pair will be the element from the other
-- 'Topic' with the nearest time stamp. The resulting 'Topic' will
-- produce a new value at the rate of the faster component 'Topic'.
everyNew :: (HasHeader a, HasHeader b) => 
            Topic IO a -> Topic IO b -> IO (Topic IO (a,b))
everyNew t1 t2 = 
  do (t1a, t1b) <- T.tee t1
     (t2a, t2b) <- T.tee t2
     let bracketLeft = pickLeft `fmap` findBrackets t1a t2a
         bracketRight = pickRight `fmap` findBrackets t2b t1b
     return . removeDups $ bracketLeft `T.merge` bracketRight
  where pickLeft ((x1,x2,_), y) = (pickNearest x1 x2 y, y)
        pickRight ((y1,y2,_), x) = (x, pickNearest y1 y2 x)

-- |The application @interpolate f t1 t2@ produces a new 'Topic' that
-- pairs every element of @t2@ with an interpolation of two temporally
-- bracketing values from @t1@. The interpolation is effected with the
-- supplied function, @f@, that is given the two values to interpolate
-- and the linear ratio to find between them. This ratio is determined
-- by the time stamp of the intervening element of @t2@.
interpolate :: (HasHeader a, HasHeader b) => 
               (a -> a -> Double -> a) -> Topic IO a -> Topic IO b -> 
               Topic IO (a,b)
interpolate f t1 t2 = interp `fmap` findBrackets t1 t2
  where interp ((x1,x2,dt),y) = let tx1 = getStamp x1
                                    ty = getStamp y
                                in (f x1 x2 (diffSeconds ty tx1 / dt), y)

-- |Batch 'Topic' values that arrive within the given time window
-- (expressed in seconds). When a value arrives, the window opens and
-- all values received within that window are returned in a list, then
-- the next value is awaited before opening the window again. Intended
-- usage is to gather approximately simultaneous events into
-- batches. Note that the times used to batch messages are arrival
-- times rather than time stamps. This is what lets us close the
-- window, rather than having to admit any message that ever arrives
-- with a compatible time stamp.
batch :: Double -> Topic IO a -> Topic IO [a]
batch timeWindow t0 = 
  Topic $ do (x,t') <- runTopic t0
             start <- getCurrentTime
             let go acc t = do now <- getCurrentTime
                               let dt = fromRational . toRational $
                                        diffUTCTime now start
                                   dMs = floor $ (timeWindow - dt) * 1000000
                               if dMs == 0
                                 then return (reverse acc, k t)
                                 else do r <- timeout dMs $ runTopic t
                                         case r of
                                           Just (x',t'') -> go (x':acc) t''
                                           Nothing -> return (reverse acc, k t)
             go [x] t'
    where k = batch timeWindow
