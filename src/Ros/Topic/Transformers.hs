-- |Functions for working with 'Topic's built around monad
-- transformers. These make it possible to, for example, repeat a
-- stateful action to produce a 'Topic''s values.
module Ros.Topic.Transformers where
import Control.Arrow
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Reader
import Ros.Topic

-- |Run a 'Topic' built around a lazy @'L.StateT' s@ monad using a
-- given initial state.
runTopicState :: Monad m => Topic (L.StateT s m) a -> s -> Topic m a
runTopicState t s = Topic $ do ((x,t'), s') <- L.runStateT (runTopic t) s
                               return (x, runTopicState t' s')

-- |Run a 'Topic' build around a strict @'S.StateT' s@ monad using a
-- given initial state.
runTopicState' :: Monad m => Topic (S.StateT s m) a -> s -> Topic m a
runTopicState' t s = Topic $ do ((x,t'), s') <- S.runStateT (runTopic t) s
                                return (x, runTopicState' t' s')

-- |Run a 'Topic' built around a 'ReaderT r' monad using a value for
-- reading.
runTopicReader :: (Functor m, Monad m) => Topic (ReaderT r m) a -> r -> Topic m a
runTopicReader t r = go t
  where go (Topic ma) = Topic $ second go `fmap` runReaderT ma r

-- |Map a monadic function over a 'Topic', in the process lifting the
-- 'Topic' into a new monad.
liftMap :: (MonadTrans t, Monad m, Monad (t m)) => 
           (a -> t m b) -> Topic m a -> Topic (t m) b
liftMap f = go
  where go (Topic ma) = Topic $ do (x,t') <- lift ma
                                   x' <- f x
                                   return (x', go t')
