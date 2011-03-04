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
module Ros.TopicStamped (everyNew, bothNew, interp) where
import Control.Applicative
import qualified Ros.Topic as T
import Ros.Topic (Topic, IterCont(..), IterContM(..), iterateTopic, iterateTopicM)
import qualified Ros.TopicUtil as T
import Ros.TopicUtil ((<+>))
import Ros.Core.Msg.HeaderSupport
import Ros.Core.RosTime

-- |Returns a 'Topic' that produces a new pair every time either of
-- the component 'Topic's produces a new value. The value of the other
-- element of the pair will be the element from the other 'Topic' with
-- the nearest time stamp. The resulting 'Topic' will produce a new
-- value at the rate of the faster component 'Topic'.
everyNew :: (HasHeader a, HasHeader b) => 
            Topic IO a -> Topic IO b -> Topic IO (a,b)
everyNew t1 t2 = let fused = T.everyNew t1 t2
                 in go <$> ((,) <$> fused <*> T.tail fused)
  where go ((px,py), (x,y))
          | getSequence px == getSequence x = (x, pickNearest py y x)
          | otherwise = (pickNearest px x y, y)
-- NOTE: This is not the intended semantics. What we want is that
-- every new element from either topic gets paired with the nearest
-- element of the other topic. If X is coming in slowly, then between
-- Xi and Xj we may have several Ym values, each of which should be
-- paired with the closest corresponding value from X. This means we
-- want to bracket each element of X /and/ bracket each element of Y.

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

-- |Given two pairs, if the first elements of the pairs bracket the
-- duplicated second element, then return a pair of the nearest first
-- element and the second element. If the second elements bracket the
-- duplicated element, return the first element and the nearest second
-- element. If there is not well-defined bracket, return 'Nothing'.
findBracket :: (HasHeader a, HasHeader b) => (a,b) -> (a,b) -> Maybe (a,b)
findBracket (sx,sy) (fx,fy)
  | qx1 == qx2 && qy1 /= qy2 = Just (sx, pickNearest sy fy sx)
  | qx1 /= qx2 && qy1 == qy2 = Just (pickNearest sx fx sy, sy)
  | otherwise = Nothing
  where qx1 = getSequence sx
        qx2 = getSequence fx
        qy1 = getSequence sy
        qy2 = getSequence fy

-- |Returns a 'Topic' that produces a new pair every time both
-- component 'Topic's have produced a new value. The composite 'Topic'
-- will produce pairs at the rate of the slower component 'Topic'
-- consisting of the most recent value from the slow 'Topic' with the
-- value from the faster 'Topic' nearest in time.
bothNew :: (HasHeader a, HasHeader b) => 
           Topic IO a -> Topic IO b -> Topic IO (a,b)
bothNew t1 t2 = iterateTopicM (go (T.everyNew t1 t2)) (T.bothNew t1 t2)
  where go fast p = let tsp = ts p
                        fast' = T.dropWhile ((<= tsp) . ts) fast
                    in do (h, fast'') <- T.uncons fast'
                          return $ IterContM (findBracket p h, go fast'')

-- |Remove consecutive duplicate pairs from a 'Topic'. Duplicates are
-- determined by the constituent messages' sequence identifiers.
removeDupPairs :: (Monad m, HasHeader a, HasHeader b) => 
                  Topic m (a,b) -> Topic m (a,b)
removeDupPairs = iterateTopic startup
  where pairSeq (a,b) = (getSequence a, getSequence b)
        startup = IterCont . (Nothing, ) . go . pairSeq
        go prevSeq curr
          | prevSeq == currSeq = IterCont (Nothing, go currSeq)
          | otherwise = IterCont (Just curr, go currSeq)
          where currSeq = pairSeq curr

-- |Return the newer timestamp from among the timestamps from each
-- element of a pair.
ts :: (HasHeader a, HasHeader b) => (a,b) -> ROSTime
ts (x,y) = max (getStamp x) (getStamp y)

-- |Update frequency is like 'everyNew', but elements of the first
-- 'Topic' are interpolated using the supplied interpolation function
-- before being paired with each elemenet of the second 'Topic'.
interp :: (HasHeader a, HasHeader b) => 
          (Double -> a -> a -> a) -> Topic IO a -> Topic IO b -> Topic IO (a,b)
interp t1 t2 = undefined
-- NOTE: Find a bracket for each element of the second Topic.
