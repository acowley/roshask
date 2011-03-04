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
module Ros.TopicStamped (everyNew, interpolate) where
import Control.Arrow (second)
import qualified Ros.Topic as T
import Ros.Topic (Topic, IterCont(..), IterContM(..), metamorph, metamorphM)
import qualified Ros.TopicUtil as T
import Ros.TopicUtil ((<+>))
import Ros.Core.Msg.HeaderSupport
import Ros.Core.RosTime

-- |Returns a 'Topic' that produces a new pair for every value
-- produced by either of the component 'Topic's. The value of the
-- other element of the pair will be the element from the other
-- 'Topic' with the nearest time stamp. The resulting 'Topic' will
-- produce a new value at the rate of the faster component 'Topic'.
everyNew :: (HasHeader a, HasHeader b) => 
            Topic IO a -> Topic IO b -> Topic IO (a,b)
everyNew t1 t2 = T.concats $ metamorph init (t1 <+> t2)
  where init :: (HasHeader a, HasHeader b) => 
                Either a b -> IterCont (Either a b) [(a,b)]
        init (Left x) = IterCont (Nothing, accY x [])
        init (Right y) = IterCont (Nothing, accX y [])
        bracket :: (HasHeader a, HasHeader b) => a -> a -> [b] -> [a]
        bracket = (map .) . pickNearest
        accX :: (HasHeader a, HasHeader b) => 
                b -> [a] -> Either a b -> IterCont (Either a b) [(a,b)]
        accX y xs (Left x) = IterCont (Nothing, accX y (x:xs))
        accX _ [] (Right y') = IterCont (Nothing, accX y' [])
        accX y xss@(x:_) (Right y')
          | getStamp x < getStamp y' = let xs = reverse xss
                                           pts = zip xs $ bracket y y' xs
                                       in IterCont $ (Just pts, accX y' [])
          | otherwise = let (post, pre) = break ((> getStamp y') . getStamp) xss
                            xs = reverse pre
                            pts = zip xs $ bracket y y' xs
                        in IterCont $ (Just pts, accX y' post)
        accY :: (HasHeader a, HasHeader b) => 
                a -> [b] -> Either a b -> IterCont (Either a b) [(a,b)]
        accY x ys (Right y) = IterCont (Nothing, accY x (y:ys))
        accY _ [] (Left x') = IterCont (Nothing, accY x' [])
        accY x yss@(y:_) (Left x')
          | getStamp y < getStamp x' = let ys = reverse yss
                                           pts = zip (bracket x x' ys) ys
                                       in IterCont $ (Just pts, accY x' [])
          | otherwise = let (post,pre) = break ((> getStamp x') . getStamp) yss
                            ys = reverse pre
                            pts = zip (bracket x x' ys) ys
                        in IterCont (Just pts, accY x' post)

{- 
NOTE: There are at least two major aspects of unsatisfying duplication
in 'everyNew'. The need for both accX and accY functions is
unfortunate, but keeping the order of the returned tuples straight is
important. The other is in the interesting clause of each, the guards
are not strictly necessary: the second ("otherwise") branch is
sufficient. The inclusion of the check for the simple case is just an
optimization.

The whole thing could be implemented by walking down each topic twice:
once with that topic as the bracketing topic, the other as it being
the provider of intervening values.
-}

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

-- |The application @interp t1 t2@ produces a new 'Topic' that pairs
-- every element of @t2@ with an interpolation of two temporally
-- bracketing values from @t1@. The interpolation is effected with the
-- supplied function that is given the two values to interpolate and
-- the linear ratio to find between them. This ratio is determined by
-- the time stamp of the intervening element of @t2@.
interpolate :: (HasHeader a, HasHeader b) => 
               (a -> a -> Double -> a) -> Topic IO a -> Topic IO b -> Topic IO (a,b)
interpolate f t1 t2 = T.concats $ metamorph startup (t1 <+> t2)
  where startup (Left x) = IterCont (Nothing, go x)
        startup (Right _) = IterCont (Nothing, startup)
        go x (Left x') = IterCont (Nothing, go x')
        go x (Right y) = IterCont (Nothing, acc x [y])
        acc x ys (Right y) = IterCont (Nothing, acc x (y:ys))
        acc x ys (Left x') = 
          let start = getStamp x
              stop = getStamp x'
              dt = diffSecs stop start
              (post,pre) = second reverse $ break ((> stop) . getStamp) ys
              dts = map ((/ dt) . (`diffSecs` start) . getStamp) pre
          in IterCont (Just $ zip (map (f x x') dts) pre, acc x' post)
        diffSecs t1 t2 = fromROSTime $ diffROSTime t1 t2
