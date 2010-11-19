{-# LANGUAGE PackageImports #-}
-- |Utility functions for working with 'Topic's. These functions
-- present familiar operations on list-like data structures, as well
-- as combinators for fusing two 'Topic's in various ways.
-- 
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude". The ambiguity may be resolved
-- using either qualification (e.g. @import qualified Ros.TopicUtil as
-- T@) or the @hiding@ clause.
module Ros.TopicUtil where
import Prelude hiding (filter)
import Control.Applicative
import Control.Arrow ((***))
import Control.Concurrent
import Control.Monad ((<=<))
import "monads-fd" Control.Monad.Trans
import Ros.Topic

-- |Return the first value produced by a 'Topic'.
head :: Functor m => Topic m a -> m a
head = fmap fst . runTopic

-- |Returns a 'Topic' containing all the values from the given 'Topic'
-- after the first.
tail :: Monad m => Topic m a -> Topic m a
tail = Topic . (runTopic . snd <=< runTopic)

-- |Returns a 'Topic' containing only those elements of the supplied
-- 'Topic' for which the given predicate returns 'True'.
filter :: Monad m => (a -> Bool) -> Topic m a -> Topic m a
filter p = go 
  where go = Topic . (aux <=< runTopic)
        aux (x, t') | p x       = return (x, go t')
                    | otherwise = runTopic $ go t'

-- |@take n t@ returns the prefix of @t@ of length @n@.
take :: Monad m => Int -> Topic m a -> m [a]
take n t = aux n t []
  where aux 0 _ acc = return (reverse acc)
        aux n t acc = do (x, t') <- runTopic t
                         aux (n-1) t' (x:acc)

-- |Run a 'Topic' for the specified number of iterations, discarding
-- the values it produces.
take_ :: Monad m => Int -> Topic m a -> m ()
take_ 0 = const $ return ()
take_ n = take_ (n-1) . snd <=< runTopic

-- |@drop n t@ returns the suffix of @t@ after the first @n@ elements.
drop :: Monad m => Int -> Topic m a -> Topic m a
drop n = Topic . aux n
  where aux 0 = runTopic
        aux n = aux (n-1) . snd <=< runTopic

-- |@dropWhile p t@ returns the suffix of @t@ after all elements
-- satisfying predicate @p@ have been dropped.
dropWhile :: Monad m => (a -> Bool) -> Topic m a -> Topic m a
dropWhile p = Topic . go
  where go = check <=< runTopic
        check (x,t) | p x       = go t
                    | otherwise = runTopic t

-- |@takeWhile p t@ returns the longest prefix (possibly empty) of @t@
-- all of whose elements satisfy the predicate @p@.
takeWhile :: Monad m => (a -> Bool) -> Topic m a -> m [a]
takeWhile p = go []
  where go acc t = do (x,t') <- runTopic t
                      if p x then go (x:acc) t' 
                             else return . reverse $ x:acc

-- |@break p t@ returns a tuple whose first element is the longest
-- prefix (possibly empty) of @t@ all of whose elements satisfy the
-- predicate @p@, and whose second element is the remainder of the
-- 'Topic'.
break :: Monad m => (a -> Bool) -> Topic m a -> m ([a], Topic m a)
break p = go []
  where go acc = check acc <=< runTopic
        check acc (x,t)
          | p x = go (x:acc) t
          | otherwise = return (reverse (x:acc), t)

-- |@splitAt n t@ returns a tuple whose first element is the prefix of
-- @t@ of length @n@, and whose second element is the remainder of the
-- 'Topic'.
splitAt :: Monad m => Int -> Topic m a -> m ([a], Topic m a)
splitAt n = go n []
  where go 0 acc t = return (reverse acc, t)
        go n acc t = do (x,t') <- runTopic t
                        go (n-1) (x:acc) t'

-- |Splits a 'Topic' into two 'Topic's: the elements of the first
-- 'Topic' all satisfy the given predicate, while none of the elements
-- of the second 'Topic' do.
partition :: (a -> Bool) -> Topic IO a -> IO (Topic IO a, Topic IO a)
partition p = fmap (filter p *** filter (not . p)) . tee

-- |Removes one level of monadic structure, projecting the values
-- produced by a 'Topic' into the monad encapsulating each step the
-- 'Topic' takes.
join :: (Functor m, Monad m) => Topic m (m a) -> Topic m a
join t = Topic $ do (x, t') <- runTopic t
                    x' <- x
                    return (x', join t')

-- |@forever t@ runs all monadic actions a 'Topic' produces. This is
-- useful for 'Topic's whose steps produce side-effects, but not
-- useful pure values.
forever :: Monad m => Topic m a -> m b
forever = forever . snd <=< runTopic

-- |Returns a 'Topic' whose values are consecutive values from the
-- original 'Topic'.
consecutive :: Monad m => Topic m a -> Topic m (a,a)
consecutive t = Topic $ do (x, t') <- runTopic t
                           runTopic $ go x t'
  where go x t' = Topic$ do (y, t'') <- runTopic t'
                            return ((x,y), go y t'')

-- |Tee a 'Topic' into two duplicate 'Topic's. Each returned 'Topic'
-- will receive all the values of the original 'Topic', while any
-- side-effect produced by each step of the original 'Topic' will
-- occur only once.
tee :: Topic IO a -> IO (Topic IO a, Topic IO a)
tee t = do c1 <- newChan
           c2 <- newChan
           let feed c = do x <- readChan c
                           return (x, Topic $ feed c)
           _ <- forkIO . forever . join $
                (\x -> writeChan c1 x >> writeChan c2 x) <$> t
           return (Topic $ feed c1, Topic $ feed c2)

-- |Apply the given function to a value from each 'Topic' in
-- lockstep. This means that the function advances down the two
-- 'Topic's without dropping any elements at the rate of the slower
-- input 'Topic'.
inStep :: (Applicative m, Monad m) => 
          (a -> b -> c) -> Topic m a -> Topic m b -> Topic m c
inStep f s1 s2 = f <$> s1 <*> s2

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
-- faster component 'Stream', and may contain duplicate consecutive
-- elements.
everyNew :: Topic IO a -> Topic IO b -> Topic IO (a,b)
everyNew t1 t2 = Topic $ warmup =<< runTopic (t1 <+> t2)
  where warmup (Left x, t)     = warmupR x =<< runTopic t
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

-- |Print all the values produced by a 'Topic'.
showTopic :: (MonadIO m, Functor m, Show a) => Topic m a -> Topic m ()
showTopic = join . fmap (liftIO . putStrLn . show)

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
