-- |The ROS Topic type.
module Ros.Topic (Topic(..)) where
import Control.Applicative
import Control.Arrow

-- A Topic is an infinite Stream with steps in a Monad.
newtype Topic m a = Topic { runTopic :: m (a, Topic m a) }

instance (Functor m, Monad m) => Functor (Topic m) where
  fmap f (Topic ma) = Topic $ fmap (f *** fmap f) ma

instance (Applicative m, Monad m) => Applicative (Topic m) where
  pure x = let t = Topic $ pure (x, t) in t
  Topic ma <*> Topic mb = Topic $ do (f,t1) <- ma
                                     (x,t2) <- mb
                                     return (f x, t1 <*> t2)
