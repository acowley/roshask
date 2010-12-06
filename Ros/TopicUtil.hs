-- |Utility functions for working with 'Topic's. These functions are
-- primarily combinators for fusing two 'Topic's in various ways.
module Ros.TopicUtil where
import Prelude hiding (dropWhile, filter, splitAt)
import Control.Applicative
import Control.Arrow ((***))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad ((<=<), when, replicateM)
import qualified Data.Foldable as F
import Ros.Topic hiding (mapM)

toList :: Topic IO a -> IO [a]
toList t = do c <- newChan
              let feed t = do (x, t') <- runTopic t
                              writeChan c x
                              feed t'
              _ <- forkIO $ feed t
              getChanContents c

fromList :: Monad m => [a] -> Topic m a
fromList (x:xs) = Topic $ return (x, fromList xs)
fromList [] = error "Ran out of list elements"

-- |Tee a 'Topic' into two duplicate 'Topic's. Each returned 'Topic'
-- will receive all the values of the original 'Topic', while any
-- side-effect produced by each step of the original 'Topic' will
-- occur only once. This version of @tee@ eagerly pulls data from the
-- original 'Topic' as soon as it is available. This behavior is
-- undesirable when lazily consuming the data stream is preferred. For
-- instance, using 'interruptible' with 'teeEager' will likely not
-- work well. However, 'teeEager' may have slightly better performance
-- than 'tee'.
teeEager :: Topic IO a -> IO (Topic IO a, Topic IO a)
teeEager t = do c1 <- newChan
                c2 <- newChan
                let feed c = do x <- readChan c
                                return (x, Topic $ feed c)
                _ <- forkIO . forever . join $
                     (\x -> writeChan c1 x >> writeChan c2 x) <$> t
                return (Topic $ feed c1, Topic $ feed c2)

-- |Tee a 'Topic' into two duplicate 'Topic's. Each returned 'Topic'
-- will receive all the values of the original 'Topic' while any
-- side-effect produced by each step of the original 'Topic' will
-- occur only once. This version of @tee@ lazily pulls data from the
-- original 'Topic' when it is first required by a consumer of either
-- of the returned 'Topic's. This behavior is crucial when lazily
-- consuming the data stream is preferred. For instance, using
-- 'interruptible' with 'tee' will allow for a chunk of data to be
-- abandoned before being fully consumed as long as neither consumer
-- has forced its way too far down the stream.
tee :: Topic IO a -> IO (Topic IO a, Topic IO a)
tee t = do c1 <- newTChanIO
           c2 <- newTChanIO
           signal <- newTVarIO True
           let feed c = do atomically $ do f <- isEmptyTChan c
                                           when f (writeTVar signal False)
                           atomically $ readTChan c
               produce t = do atomically $ readTVar signal >>= flip when retry
                              (x,t') <- runTopic t
                              atomically $ writeTChan c1 x >>
                                           writeTChan c2 x >>
                                           writeTVar signal True
                              produce t'
           _ <- forkIO $ produce t
           return (unfold (feed c1), unfold (feed c2))

-- |Fan out one 'Topic' out to a number of duplicate 'Topic's, each of
-- which will produce the same values. Side effects caused by the
-- original 'Topic''s production will occur only once.
fan :: Int -> Topic IO a -> IO [Topic IO a]
fan n t = do cs <- replicateM n newTChanIO
             signal <- newTVarIO True
             let feed c = do atomically $ do f <- isEmptyTChan c
                                             when f (writeTVar signal False)
                             atomically $ readTChan c
                 produce t = do atomically $ readTVar signal >>= flip when retry
                                (x,t') <- runTopic t
                                atomically $ mapM_ (flip writeTChan x) cs >>
                                             writeTVar signal True
                                produce t'
             _ <- forkIO $ produce t
             return $ map (unfold . feed) cs

-- |Splits a 'Topic' into two 'Topic's: the elements of the first
-- 'Topic' all satisfy the given predicate, while none of the elements
-- of the second 'Topic' do.
partition :: (a -> Bool) -> Topic IO a -> IO (Topic IO a, Topic IO a)
partition p = fmap (filter p *** filter (not . p)) . tee

-- |Returns a 'Topic' whose values are consecutive values from the
-- original 'Topic'.
consecutive :: Monad m => Topic m a -> Topic m (a,a)
consecutive t = Topic $ do (x, t') <- runTopic t
                           runTopic $ go x t'
  where go x t' = Topic$ do (y, t'') <- runTopic t'
                            return ((x,y), go y t'')

-- |Interleave two 'Topic's. Items from each component 'Topic' will be
-- tagged with an 'Either' constructor and added to the combined
-- 'Topic' as they become available.
(<+>) :: Topic IO a -> Topic IO b -> Topic IO (Either a b)
(<+>) t1 t2 = Topic $ do c <- newChan
                         let aux = do x <- readChan c
                                      return (x, Topic aux)
                             feed t = do (x,t') <- runTopic t
                                         writeChan c x
                                         feed t'
                         _ <- forkIO $ feed (fmap Left t1)
                         _ <- forkIO $ feed (fmap Right t2)
                         aux
infixl 7 <+>

-- |Returns a 'Topic' that produces a new pair every time either of
-- the component 'Topic's produces a new value. The value of the
-- other element of the pair will be the newest available value. The
-- resulting 'Topic' will produce a new value at the rate of the
-- faster component 'Topic', and may contain duplicate consecutive
-- elements.
everyNew :: Topic IO a -> Topic IO b -> Topic IO (a,b)
everyNew t1 t2 = Topic $ warmup =<< runTopic (t1 <+> t2)
  where warmup (Left x, t)     = warmupR x =<< runTopic t
        warmup (Right y, t)    = warmupL y =<< runTopic t
        warmupR _ (Left x, t)  = warmupR x =<< runTopic t
        warmupR x (Right y, t) = return ((x,y), Topic $ runTopic t >>= go x y)
        warmupL _ (Right y, t) = warmupL y =<< runTopic t
        warmupL y (Left x, t)  = return ((x,y), Topic $ runTopic t >>= go x y)
        go _ y (Left x, t)     = return ((x,y), Topic $ runTopic t >>= go x y)
        go x _ (Right y, t)    = return ((x,y), Topic $ runTopic t >>= go x y)

-- |Returns a 'Topic' that produces a new pair every time both of the
-- component 'Topic's have produced a new value. The composite
-- 'Topic' will produce pairs at the rate of the slower component
-- 'Topic' consisting of the most recent value from each 'Topic'.
bothNew :: Topic IO a -> Topic IO b -> Topic IO (a,b)
bothNew t1 t2 = Topic $ warmup =<< runTopic (t1 <+> t2)
  where warmup (v,t) = go v =<< runTopic t
        go (Left _) (l@(Left _), t) = go l =<< runTopic t
        go (Left x) (Right y, t) = return ((x,y), Topic $ warmup =<< runTopic t)
        go (Right _) (r@(Right _), t) = go r =<< runTopic t
        go (Right y) (Left x, t) = return ((x,y), Topic $ warmup =<< runTopic t)

-- |Merge two 'Topic's into one. The items from each component
-- 'Topic' will be added to the combined 'Topic' as they become
-- available.
merge :: Topic IO a -> Topic IO a -> Topic IO a
merge t1 t2 = either id id <$> t1 <+> t2

-- |Apply a function to each consecutive pair of elements from a 'Topic'.
finiteDifference :: (Functor m, Monad m) => (a -> a -> b) -> Topic m a -> Topic m b
finiteDifference f = fmap (uncurry f) . consecutive

-- |Compute a running \"average\" of a 'Topic' using a user-provided
-- normalization function applied to the sum of products. The
-- arguments are a constat @alpha@ that is used to scale the current
-- average, a constant @invAlpha@ used to scale the newest value, a
-- function for adding two scaled values, a function for scaling
-- input values, a function for normalizing the sum of scaled values,
-- and finally the stream to average. Parameterizing over all the
-- arithmetic to this extent allows for the use of denormalizing
-- scaling factors, as might be used to keep all arithmetic
-- integral. An example would be scaling the average by the integer
-- 7, the new value by the integer 1, then normalizing by dividing
-- the sum of scaled values by 8.
weightedMeanNormalized :: Monad m =>
                          n -> n -> (b -> b -> c) -> (n -> a -> b) -> 
                          (c -> a) -> Topic m a -> Topic m a
weightedMeanNormalized alpha invAlpha plus scale normalize = Topic . warmup
    where warmup = uncurry go <=< runTopic
          go avg t = do (x,t') <- runTopic t
                        let !avg' = normalize $ plus (scale alpha avg) 
                                                     (scale invAlpha x)
                        return (avg', Topic $ go avg' t')
{-# INLINE weightedMeanNormalized #-}

-- |Perform numerical integration of a 'Topic' using Simpson's rule
-- applied at three consecutive points. This requires a function for
-- adding values from the 'Topic', and a function for scaling values
-- by a fractional number.
simpsonsRule :: (Monad m, Fractional n) => 
                (a -> a -> a) -> (n -> a -> a) -> Topic m a -> Topic m a
simpsonsRule plus scale t = Topic $ do ([x,y], t') <- splitAt 2 t
                                       go x y t'
  where go x y t = do (z,t') <- runTopic t
                      return (simpson x y z, Topic $ go y z t')
        simpson a mid b = scale c $ plus (plus a (scale 4 mid)) b
        c = 1 / 3
{-# INLINE simpsonsRule #-}

-- |Compute a running \"average\" of a 'Topic'. The application
-- @weightedMean alpha plus scale t@ sums the product of @alpha@ and
-- the current average with the product of @1 - alpha@ and the newest
-- value produced by 'Topic' @t@. The addition and scaling operations
-- are performed using the supplied @plus@ and @scale@ functions.
weightedMean :: (Monad m, Num n) => 
                n -> (a -> a -> a) -> (n -> a -> a) -> Topic m a -> Topic m a
weightedMean alpha plus scale = weightedMean2 alpha (1 - alpha) plus scale
{-# INLINE weightedMean #-}

-- |Compute a running \"average\" of a 'Topic'. The application
-- @weightedMean2 alpha invAlpha plus scale t@ sums the product of
-- @alpha@ and the current average with the product of @invAlpha@ and
-- the newest value produced by 'Topic' @t@. The addition and scaling
-- operations are performed using the supplied @plus@ and @scale@
-- functions.
weightedMean2 :: Monad m =>
                 n -> n -> (a -> a -> a) -> (n -> a -> a) -> Topic m a -> Topic m a
weightedMean2 alpha invAlpha plus scale = Topic . warmup
    where warmup = uncurry go <=< runTopic
          go avg t = do (x, t') <- runTopic t
                        let !savg = scale alpha avg
                            !sx = scale invAlpha x
                            !avg' = plus savg sx
                        return (avg', Topic $ go avg' t')
{-# INLINE weightedMean2 #-}

-- |Use a 'Topic' of functions to filter a 'Topic' of values. The
-- application @filterBy t1 t2@ causes each function from 'Topic' @t1@
-- to be applied to values produced by @t2@ until it returns
-- 'True'. At that point, the 'filterBy' application produces the
-- accepted value of the @t2@ and moves on to the next function from
-- @t1@ which is applied to the rest of @t2@ in the same manner.
filterBy :: Monad m => Topic m (a -> Bool) -> Topic m a -> Topic m a
filterBy tf tx = Topic $ do (f, tf') <- runTopic tf
                            (x, tx') <- uncons $ dropWhile (not . f) tx
                            return (x, filterBy tf' tx')

-- |Produce elements of the first 'Topic' no faster than elements of
-- the second 'Topic' are produced.
gate :: (Applicative m, Monad m) => Topic m a -> Topic m b -> Topic m a
gate t1 t2 = const <$> t1 <*> t2

-- |Flatten a 'Topic' of 'F.Foldable' values. For example, turn a
-- @Topic m [a]@ of finite lists into a @Topic a@ by taking each
-- element from each list in sequence.
concats :: (Monad m, F.Foldable f) => Topic m (f a) -> Topic m a
concats t = Topic $ do (x, t') <- runTopic t
                       F.foldr (\x z -> return (x, Topic z)) 
                               (runTopic $ concats t') 
                               x

-- |Flatten a 'Topic' of 'F.Foldable' values such that old values are
-- discarded as soon as the original 'Topic' produces a new
-- 'F.Foldable'.
interruptible :: F.Foldable t => Topic IO (t a) -> Topic IO a
interruptible s = Topic $
    do feeder <- newEmptyMVar         -- Active feeder thread
       latestItem <- newEmptyMVar     -- Next available item
       let feedItems ys = do ft <- tryTakeMVar feeder
                             maybe (return ()) killThread ft
                             t <- forkIO $ F.traverse_ (putMVar latestItem) ys
                             putMVar feeder t 
           watchForItems t = do (x,t') <- runTopic t
                                feedItems x 
                                watchForItems t'
           getAll = do x <- takeMVar latestItem
                       return (x, Topic getAll)
       _ <- forkIO $ watchForItems s
       getAll


{-
nats :: Topic IO Int
nats = aux 0
  where aux n = Topic $ threadDelay 2000000 >> return (n, aux (n+1))

chars :: Topic IO Char
chars = aux 'a'
  where aux 'z' = Topic $ threadDelay 200000 >> return ('z', aux 'a')
        aux c = Topic $ threadDelay 200000 >> return (c, aux (toEnum (fromEnum c + 1)))


fastAndSlow = take_ 21 . showTopic $ chars <+> nats
fastAndSlow2 = take_ 20 . showTopic $ everyNew chars nats
testBothNew = take_ 10 . showTopic $ bothNew chars nats
testDiffs = take_ 10 . showTopic $ finiteDifference (flip (-)) nats
-}
