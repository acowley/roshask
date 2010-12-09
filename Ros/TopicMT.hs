{-# LANGUAGE PackageImports #-}
-- |Functions for working with 'Topic's built around monad
-- transformers. These make it possible to, for example, repeat a
-- stateful action to produce a 'Topic''s values.
module Ros.TopicMT where
import Control.Arrow
import "monads-fd" Control.Monad.State.Lazy
import "monads-fd" Control.Monad.Reader
import Ros.Topic

runTopicState :: Monad m => Topic (StateT s m) a -> s -> Topic m a
runTopicState t s = Topic $ do ((x,t'), s') <- runStateT (runTopic t) s
                               return (x, runTopicState t' s')

runTopicReader :: (Functor m, Monad m) => Topic (ReaderT r m) a -> r -> Topic m a
runTopicReader t r = go t
  where go (Topic ma) = Topic $ second go `fmap` runReaderT ma r
