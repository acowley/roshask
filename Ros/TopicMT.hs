{-# LANGUAGE PackageImports #-}
-- |Functions for working with 'Topic's built around monad
-- transformers. These make it possible to, for example, repeat a
-- stateful action to produce a 'Topic''s values.
module Ros.TopicMT where
import Control.Arrow
import "monads-fd" Control.Monad.State.Lazy
import "monads-fd" Control.Monad.Reader
import Ros.Topic

-- |Run a 'Topic' built around a 'StateT s' monad using a given
-- initial state.
runTopicState :: Monad m => Topic (StateT s m) a -> s -> Topic m a
runTopicState t s = Topic $ do ((x,t'), s') <- runStateT (runTopic t) s
                               return (x, runTopicState t' s')

-- |Run a 'Topic' built around a 'ReaderT r' monad using a value for
-- reading.
runTopicReader :: (Functor m, Monad m) => Topic (ReaderT r m) a -> r -> Topic m a
runTopicReader t r = go t
  where go (Topic ma) = Topic $ second go `fmap` runReaderT ma r

-- |Map a function over a 'Topic', in the process lifting the 'Topic'
-- into a new monad.
liftMap :: (MonadTrans t, Monad m, Monad (t m)) => 
           (a -> t m b) -> Topic m a -> Topic (t m) b
liftMap f = go
  where go (Topic ma) = Topic $ do (x,t') <- lift ma
                                   x' <- f x
                                   return (x', go t')
