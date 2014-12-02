{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- |The ROS Topic type and basic operations on Topics.
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude". The ambiguity may be resolved
-- using either qualification (e.g. @import qualified Ros.TopicUtil as
-- T@), an explicit import list, or a @hiding@ clause.
module Ros.Topic where
#if __GLASGOW_HASKELL__ >= 710
import Prelude hiding (join)
#endif
import Control.Applicative
import Control.Arrow ((***), second)
import Control.Monad ((<=<), (>=>))
import Control.Monad.IO.Class

-- |A Topic is an infinite stream of values that steps between values
-- in a 'Monad'.
newtype Topic m a = Topic { runTopic :: m (a, Topic m a) }

instance Functor m => Functor (Topic m) where
  fmap f (Topic ma) = Topic $ fmap (f *** fmap f) ma

instance Applicative m => Applicative (Topic m) where
  pure x = let t = Topic $ pure (x, t) in t
  Topic ma <*> Topic mb = Topic $ uncurry (***) . (($) *** (<*>)) <$> ma <*> mb

-- |Return the first value produced by a 'Topic'.
head :: Functor m => Topic m a -> m a
head = fmap fst . runTopic

-- |Return the first value produced by a 'Topic' along with the
-- remaining 'Topic' data.
uncons :: Topic m a -> m (a, Topic m a)
uncons = runTopic

-- |Force evaluation of a topic until it produces a value.
force :: Monad m => Topic m a -> m (Topic m a)
force = uncons >=> return . Topic . return

-- |Prepend a single item to the front of a 'Topic'.
cons :: Monad m => a -> Topic m a -> Topic m a
cons x t = Topic $ return (x, t)

-- |Returns a 'Topic' containing all the values from the given 'Topic'
-- after the first.
tail :: Monad m => Topic m a -> Topic m a
tail = Topic . (runTopic . snd <=< runTopic)

-- |Return a 'Topic' of all the suffixes of a 'Topic'.
tails :: Monad m => Topic m a -> Topic m (Topic m a)
tails t = Topic $ do (x,t') <- runTopic t
                     return (Topic $ return (x,t'), tails t')

-- |Returns a 'Topic' containing only those elements of the supplied
-- 'Topic' for which the given predicate returns 'True'.
filter :: Monad m => (a -> Bool) -> Topic m a -> Topic m a
filter p = metamorph go
  where go x | p x = yield x go
             | otherwise = skip go
-- filter p = go 
--   where go = Topic . (aux <=< runTopic)
--         aux (x, t') | p x       = return (x, go t')
--                     | otherwise = runTopic $ go t'

-- |@take n t@ returns the prefix of @t@ of length @n@.
take :: Monad m => Int -> Topic m a -> m [a]
take = aux []
  where aux acc 0 _ = return (reverse acc)
        aux acc n' t = do (x, t') <- runTopic t
                          aux (x:acc) (n'-1) t'

-- |Run a 'Topic' for the specified number of iterations, discarding
-- the values it produces.
take_ :: Monad m => Int -> Topic m a -> m ()
take_ 0 = const $ return ()
take_ n = take_ (n-1) . snd <=< runTopic

-- |@drop n t@ returns the suffix of @t@ after the first @n@ elements.
drop :: Monad m => Int -> Topic m a -> Topic m a
drop = (Topic .) . aux
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
splitAt = go []
  where go acc 0 t = return (reverse acc, t)
        go acc n t = do (x,t') <- runTopic t
                        go (x:acc) (n-1) t'

-- |Returns a 'Topic' that includes only the 'Just' values from the
-- given 'Topic'.
catMaybes :: Monad m => Topic m (Maybe a) -> Topic m a
catMaybes = metamorph go
  where go = maybe (skip go) (flip yield go)
-- catMaybes (Topic ma) = Topic $ ma >>= aux
--   where aux (Nothing, t') = runTopic $ catMaybes t'
--         aux (Just x, t')  = return (x, catMaybes t')

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

-- |A pair of an optional value and a continuation for producing more
-- such pairs. This type is used by 'metamorph' to implement a
-- streaming @unfold . fold@ composition.
newtype IterCont a b = IterCont (Maybe b, a -> IterCont a b)

instance Functor (IterCont a) where
  fmap f (IterCont (x, k)) = IterCont (fmap f x, fmap f . k)

-- |A pair of an optional value and a continuation with effects for
-- producing more such pairs. This type is used by 'metamorphM' to
-- implement a streaming @unfold . fold@ composition.
newtype IterContM m a b = IterContM (Maybe b, a -> m (IterContM m a b))

instance Monad m => Functor (IterContM m a) where
  fmap f (IterContM (x, k)) = IterContM (fmap f x,  return . fmap f <=< k)

-- |Yield a value and a continuation in a metamorphism (used with
-- 'metamorph').
yield :: b -> (a -> IterCont a b) -> IterCont a b
yield = curry IterCont . Just

-- |Do not yield a value, but provide a continuation in a metamorphism
-- (used with 'metamorph').
skip :: (a -> IterCont a b) -> IterCont a b
skip = curry IterCont Nothing

-- |Yield a value and a continuation in a monad as part of a monadic
-- metamorphism (used with 'metamorphM').
yieldM :: Monad m => b -> (a -> m (IterContM m a b)) -> m (IterContM m a b)
yieldM = (return .) . curry IterContM . Just

-- |Do not yield a value, but provide a continuation in a metamorphism
-- (used with 'metamorphM').
skipM :: Monad m => (a -> m (IterContM m a b)) -> m (IterContM m a b)
skipM = return . curry IterContM Nothing

-- |A metamorphism (cf. Jeremy Gibbons) on 'Topic's. This is an
-- /unfold/ following a /fold/ (i.e. @unfoldr . foldl@), with the
-- expectation that partial results of the /unfold/ may be returned
-- before the /fold/ is completed. The supplied function produces a
-- optional value and a continuation when applied to an element of the
-- first 'Topic'. The value is returned by the new 'Topic' if it is
-- not 'Nothing', and the continuation is used to produce the rest of
-- the returned 'Topic'.
metamorph :: Monad m => (a -> IterCont a b) -> Topic m a -> Topic m b
metamorph f t = Topic $ do (x,t') <- runTopic t
                           let IterCont (x', f') = f x
                           case x' of
                             Nothing -> runTopic $ metamorph f' t'
                             Just x'' -> return (x'', metamorph f' t')

-- |Similar to 'metamorph', but the metamorphism may have effects.
metamorphM :: Monad m => (a -> m (IterContM m a b)) -> Topic m a -> Topic m b
metamorphM f t = Topic $ do (x,t') <- runTopic t
                            IterContM (x', f') <- f x
                            case x' of
                              Nothing -> runTopic $ metamorphM f' t'
                              Just x'' -> return (x'', metamorphM f' t')

-- |Fold two functions along a 'Topic' collecting their productions in
-- a new 'Topic'.
bimetamorph :: Monad m =>
               (a -> IterCont a b) -> (a -> IterCont a b) ->
               Topic m a -> Topic m b
bimetamorph f g t = Topic $ do (x,t') <- runTopic t
                               let IterCont (y, f') = f x
                                   IterCont (z, g') = g x
                               aux y . aux z . runTopic $ bimetamorph f' g' t'
  where aux = maybe id (\x y -> return (x, Topic y))

-- |Fold two monadic functions along a 'Topic' collecting their
-- productions in a new 'Topic'.
bimetamorphM :: Monad m =>
                (a -> m (IterContM m a b)) -> (a -> m (IterContM m a b)) ->
                Topic m a -> Topic m b
bimetamorphM f g t = Topic $ do (x,t') <- runTopic t
                                IterContM (y, f') <- f x
                                IterContM (z, g') <- g x
                                aux y . aux z . runTopic $ bimetamorphM f' g' t'
  where aux = maybe id (\x y -> return (x, Topic y))

-- |Fold two functions along a 'Topic' collecting and tagging their
-- productions in a new 'Topic'.
bimetamorphE :: Monad m => 
                (a -> IterCont a b) -> (a -> IterCont a c) ->
                Topic m a -> Topic m (Either b c)
bimetamorphE f g t = bimetamorph (fmap Left . f) (fmap Right . g) t

-- |Fold two monadic functions along a 'Topic' collecting and tagging
-- their productions in a new 'Topic'.
bimetamorphME :: Monad m => 
                 (a -> m (IterContM m a b)) -> (a -> m (IterContM m a c)) ->
                 Topic m a -> Topic m (Either b c)
bimetamorphME f g t = 
  bimetamorphM (return . fmap Left <=< f) (return . fmap Right <=< g) t

-- |Removes one level of monadic structure from the values a 'Topic'
-- produces.
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

-- |Map a monadic action of a 'Topic' purely for its side
-- effects. This function will never return.
mapM_ :: Monad m => (a -> m ()) -> Topic m a -> m ()
mapM_ f = go
  where go = uncurry (>>) . (f *** go) <=< runTopic

-- |A left-associative scan of a 'Topic' is a fold whose every
-- intermediate value is produced as a value of a new 'Topic'.
scan :: Monad m => (a -> b -> a) -> a -> Topic m b -> Topic m a
scan f z = metamorph (go z)
  where go acc x = let x' = f acc x in yield x' (go x')

-- |Print all the values produced by a 'Topic'.
showTopic :: (MonadIO m, Functor m, Show a) => Topic m a -> Topic m ()
showTopic = join . fmap (liftIO . putStrLn . show)

