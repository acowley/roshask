-- |The ROS Topic type and basic operations on Topics.
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude". The ambiguity may be resolved
-- using either qualification (e.g. @import qualified Ros.TopicUtil as
-- T@), an explicit import list, or a @hiding@ clause.
module Ros.Topic where
import Control.Applicative
import Control.Arrow
import Control.Monad ((<=<))
import Control.Monad.IO.Class

-- |A Topic is an infinite stream of values that steps between values
-- in a 'Monad'.
newtype Topic m a = Topic { runTopic :: m (a, Topic m a) }

instance (Functor m, Monad m) => Functor (Topic m) where
  fmap f (Topic ma) = Topic $ fmap (f *** fmap f) ma

instance (Applicative m, Monad m) => Applicative (Topic m) where
  pure x = let t = Topic $ pure (x, t) in t
  Topic ma <*> Topic mb = Topic $ do (f,t1) <- ma
                                     (x,t2) <- mb
                                     return (f x, t1 <*> t2)

-- |Return the first value produced by a 'Topic'.
head :: Functor m => Topic m a -> m a
head = fmap fst . runTopic

-- |Return the first value produced by a 'Topic' along with the
-- remaining 'Topic' data.
uncons :: Topic m a -> m (a, Topic m a)
uncons = runTopic

-- |Prepend a single item to the front of a 'Topic'.
cons :: Monad m => a -> Topic m a -> Topic m a
cons x t = Topic $ return (x, t)

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
                    | otherwise = return (x, t)

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

-- |Repeatedly execute a monadic action feeding the values into a
-- 'Topic'.
repeatM :: Monad m => m a -> Topic m a
repeatM action = go
  where go = Topic $ action >>= \x -> return (x, go)

-- |Build a 'Topic' from a seed value. The supplied function is
-- applied to the seed value to produce both a value that goes into
-- the 'Topic' and a new seed value for the next recursive call.
unfold :: Functor m => (b -> m (a,b)) -> b -> Topic m a
unfold f z0 = go z0
  where go z = Topic $ second go <$> f z

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

-- |Map a monadic action over a 'Topic'.
mapM :: (Functor m, Monad m) => (a -> m b) -> Topic m a -> Topic m b
mapM = (join .) . fmap

-- |Print all the values produced by a 'Topic'.
showTopic :: (MonadIO m, Functor m, Show a) => Topic m a -> Topic m ()
showTopic = join . fmap (liftIO . putStrLn . show)
