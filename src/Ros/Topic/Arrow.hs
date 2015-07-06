{-# LANGUAGE FlexibleInstances #-}
-- |The TopicArrow type and wrappers for Topic operations.
--
-- This module provide an arrow interface for Topic computations
-- alternatively monadic inteface @Ros.Topic@.
--
-- /Note/: Many of these operations have the same names as similar
-- operations in the "Prelude" and @Ros.Topic@. The ambiguity may be resolved
-- using either qualification (e.g. @import qualified Ros.Topic.Arrow as
-- A@), an explicit import list, or a @hiding@ clause.
module Ros.Topic.Arrow where

import Ros.Internal.RosBinary (RosBinary)
import Ros.Internal.Msg.MsgInfo (MsgInfo)
import Ros.Node (Node, Topic, TopicName, Subscribe, Advertise,
                 subscribe, advertise, advertiseBuffered)
import qualified Ros.Topic.Util as U
import qualified Ros.Topic as T

import Data.AdditiveGroup (AdditiveGroup)
import Data.Typeable (Typeable)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid)

import Control.Monad.IO.Class
import Control.Applicative
import Control.Category
import Control.Arrow

import Prelude hiding (id, (.))

-- |A @TopicArrow@ type is a map of Topic.
newtype TopicArrow m a b = TopicArrow
  { runTopicArrow :: Topic m a -> Topic m b }

-- |@TopicArrow@ is a category
instance Category (TopicArrow m) where
  id                          = TopicArrow id
  TopicArrow f . TopicArrow g = TopicArrow (f . g)

-- |@TopicArrow@ is an arrow
instance (Functor m, Applicative m) => Arrow (TopicArrow m) where
  arr                  = TopicArrow . fmap
  first (TopicArrow f) = TopicArrow $ (\(a, b) -> (,) <$> a <*> b)
                                      <<< f . (fst <$>) &&& (snd <$>)

-- |Simple Subscribe instance
instance Subscribe (TopicArrow IO ()) where
  subscribe n =
    (TopicArrow . const) <$> subscribe n

-- |Simple Advertise instance
instance Advertise (TopicArrow IO ()) where
  advertiseBuffered c n a =
    let emptyTopic = T.repeatM $ return ()
     in advertiseBuffered c n $ runTopicArrow a emptyTopic

-- |Connect two ROS topic's by name with an arrow
class Arrow a => Connect a where
  connect :: (RosBinary b, MsgInfo b, Typeable b,
              RosBinary c, MsgInfo c, Typeable c)
          => TopicName -> TopicName -> a b c -> Node ()

instance Connect (->) where
  connect a b = connect' a b . linkFn

instance Connect (TopicArrow IO) where
  connect a b = connect' a b . linkTA

linkFn :: (a -> b) -> Topic IO a -> Topic IO b
linkFn = fmap

linkTA :: TopicArrow IO a b -> TopicArrow IO () a -> TopicArrow IO () b
linkTA = (<<<)

connect' :: (Subscribe s, Advertise a,
             RosBinary b, MsgInfo b, Typeable b,
             RosBinary c, MsgInfo c, Typeable c)
         => TopicName -> TopicName -> (s b -> a c) -> Node ()
connect' n1 n2 f =
  subscribe n1 >>= advertise n2 . f

--
-- |@Ros.Topic@ operations wrappers
--

--break

catMaybes :: Monad m => TopicArrow m (Maybe a) a
catMaybes = TopicArrow T.catMaybes

cons :: Monad m => a -> TopicArrow m a a
cons = TopicArrow . T.cons

drop :: Monad m => Int -> TopicArrow m a a
drop = TopicArrow . T.drop

dropWhile :: Monad m => (a -> Bool) -> TopicArrow m a a
dropWhile = TopicArrow . T.dropWhile

filter :: Monad m => (a -> Bool) -> TopicArrow m a a
filter = TopicArrow . T.filter

-- force
-- forever
-- head
-- join

mapM :: (Functor m, Monad m) => (a -> m b) -> TopicArrow m a b
mapM = TopicArrow . T.mapM

-- mapM_

repeatM :: Monad m => m a -> TopicArrow m () a
repeatM = TopicArrow . const . T.repeatM

scan :: Monad m => (a -> b -> a) -> a -> TopicArrow m b a
scan f = TopicArrow . T.scan f

-- showTopic
-- splitAt

tail :: Monad m => TopicArrow m a a
tail = TopicArrow T.tail

-- tails
-- take
-- takeWhile
-- take_
-- uncons

unfold :: Functor m => (b -> m (a, b)) -> b -> TopicArrow m () a
unfold f = TopicArrow . const . T.unfold f

--
-- |@Ros.Topic.Util@ operations wrappers
--

toList :: TopicArrow IO () a -> IO [a]
toList = U.toList . flip runTopicArrow (T.repeatM (return ()))

fromList :: Monad m => [a] -> TopicArrow m () a
fromList = TopicArrow . const . U.fromList

-- tee
-- teeEager
-- fan
-- share

topicRate :: (Functor m, MonadIO m) => Double -> TopicArrow m a a
topicRate = TopicArrow . U.topicRate

-- partition

consecutive :: Monad m => TopicArrow m a (a, a)
consecutive = TopicArrow U.consecutive

-- <+>
-- everyNew
-- bothNew
-- firstThenSecond

leftThenRight :: Monad m => TopicArrow m (Either a b) (a, b)
leftThenRight = TopicArrow U.leftThenRight

-- merge

finiteDifference :: (Functor m, Monad m) => (a -> a -> b) -> TopicArrow m a b
finiteDifference = TopicArrow . U.finiteDifference


weightedMeanNormalized :: Monad m =>
                          n -> n -> (b -> b -> c) -> (n -> a -> b) ->
                          (c -> a) -> TopicArrow m a a
weightedMeanNormalized alpha invAlpha plus scale normalize =
  TopicArrow $ U.weightedMeanNormalized alpha invAlpha plus scale normalize

simpsonsRule :: (Monad m, Fractional n) =>
                (a -> a -> a) -> (n -> a -> a) -> TopicArrow m a a
simpsonsRule plus scale = TopicArrow $ U.simpsonsRule plus scale


weightedMean :: (Monad m, Num n) =>
                n -> (a -> a -> a) -> (n -> a -> a) -> TopicArrow m a a
weightedMean alpha plus scale = weightedMean2 alpha (1 - alpha) plus scale


weightedMean2 :: Monad m =>
                 n -> n -> (a -> a -> a) -> (n -> a -> a) -> TopicArrow m a a
weightedMean2 alpha invAlpha plus scale =
  TopicArrow $ U.weightedMean2 alpha invAlpha plus scale

-- filterBy
-- gate

concats :: (Monad m, Foldable f) => TopicArrow m (f a) a
concats = TopicArrow U.concats

-- interruptible
-- forkTopic

slidingWindow :: (Monad m, Monoid a) => Int -> TopicArrow m a a
slidingWindow = TopicArrow . U.slidingWindow

slidingWindowG :: (Monad m, AdditiveGroup a) => Int -> TopicArrow m a a
slidingWindowG = TopicArrow . U.slidingWindowG

topicOn :: (Applicative m, Monad m) =>
           (a -> b) -> (a -> c -> d) -> m (b -> m c) -> TopicArrow m a d
topicOn proj inj trans = TopicArrow $ U.topicOn proj inj trans

subsample :: Monad m => Int -> TopicArrow m a a
subsample = TopicArrow . U.subsample
