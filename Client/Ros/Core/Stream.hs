-- |The core 'Stream' type used to represent Topic-based
-- communication. A 'Stream' is simply an infinite list. This module
-- is a fork of Wouter Swierstra's Stream package (v0.4.1) with the
-- lazy pattern matches replaced by strict pattern matches, the
-- dependencies on QuickCheck and lazysmallcheck removed, and a
-- 'Foldable' instance added.
module Ros.Core.Stream
   (
   -- * The type of streams
     Stream(..)
   -- * Basic functions
   , (<:>)
   , head
   , tail
   , inits
   , tails
   -- * Stream transformations
   , map
   , intersperse
   , interleave
   , scan
   , scan'
   , scan1
   , scan1'
   , transpose
   -- * Building streams
   , iterate
   , repeat
   , cycle
   , unfold
   -- * Extracting sublists
   , take
   , drop
   , splitAt
   , takeWhile
   , dropWhile
   , span
   , break
   , filter
   , partition
   , group
   -- * Sublist predicates
   , isPrefixOf
   -- * Indexing streams
   , (!!) 
   , elemIndex
   , elemIndices
   , findIndex
   , findIndices
   -- * Zipping and unzipping streams
   , zip
   , zipWith
   , unzip
   -- * Functions on streams of characters
   , words
   , unwords
   , lines
   , unlines
   -- * Converting to and from an infinite list
   , toList
   , fromList
   )
   where

import Prelude hiding (head, tail, map, scanl, scanl1,
  iterate, take, drop, takeWhile,
  dropWhile, repeat, cycle, filter, (!!), zip, unzip,
  zipWith,words,unwords,lines,unlines, break, span, splitAt, foldr)

import Control.Applicative
import Data.Monoid (mappend)
import Data.Char (isSpace)
import Data.Foldable (Foldable, foldr, foldMap)

-- | An infinite sequence.
--
-- /Beware/: If you use any function from the @ Eq @ or @ Ord @
-- class to compare two equal streams, these functions will diverge.

data Stream a = Cons a (Stream a) deriving (Eq, Ord)

infixr 5 `Cons`

instance Functor Stream where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Stream where
  pure = repeat
  (<*>) = zipWith ($)

instance Monad Stream where
  return = repeat
  xs >>= f = join (fmap f xs)
    where
      join :: Stream (Stream a) -> Stream a
      join (Cons xs xss) = Cons (head xs) (join (map tail xss))

-- |The obvious 'Foldable' instance for 'Stream'. Note that 'Stream's
-- are infinite, so many operations on 'Stream's will diverge
-- (e.g. computing the sum).
instance Foldable Stream where
    foldr f z (Cons x xs) = f x (foldr f z xs)
    foldMap f (Cons x xs) = f x `mappend` foldMap f xs

-- | A Show instance for Streams that takes the right associativity into
-- account and so doesn't put parenthesis around the tail of the Stream.
-- Note that 'show' returns an infinite 'String'.
instance Show a => Show (Stream a) where
  showsPrec p (Cons x xs) = 
    showParen (p > consPrecedence)   $
    showsPrec (consPrecedence + 1) x .
    showString " <:> "               .
    showsPrec consPrecedence xs
    where
      consPrecedence = 5 :: Int
                             
infixr 5 <:>
-- | The @ \<:\> @ operator is an infix version of the 'Cons'
-- constructor.
(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons

-- | Extract the first element of the sequence.
head :: Stream a -> a
head (Cons x _ ) = x

-- | Extract the sequence following the head of the stream.
tail :: Stream a -> Stream a
tail (Cons _ xs) = xs

-- | The 'inits' function takes a stream @xs@ and returns all the
-- finite prefixes of @xs@.
--
-- Note that this 'inits' is lazier then @Data.List.inits@:
--
-- > inits _|_ = [] ::: _|_
--
-- while for @Data.List.inits@:
--
-- > inits _|_ = _|_

inits :: Stream a -> Stream ([a])
inits xs = Cons [] (fmap (head xs :) (inits (tail xs)))

-- | The 'tails' function takes a stream @xs@ and returns all the
-- suffixes of @xs@.
tails :: Stream a -> Stream (Stream a)
tails xs = Cons xs (tails (tail xs))

-- | Apply a function uniformly over all elements of a sequence.
map :: (a -> b) -> Stream a -> Stream b
map f (Cons x xs) = Cons (f x) (map f xs)

-- | 'intersperse' @y@ @xs@ creates an alternating stream of
-- elements from @xs@ and @y@.
intersperse :: a -> Stream a -> Stream a
intersperse y (Cons x xs) = Cons x (Cons y (intersperse y xs))

-- | Interleave two Streams @xs@ and @ys@, alternating elements
-- from each list.
--
-- > [x1,x2,...] `interleave` [y1,y2,...] == [x1,y1,x2,y2,...]
interleave :: Stream a -> Stream a -> Stream a
interleave (Cons x xs) ys = Cons x (interleave ys xs)

-- | 'scan' yields a stream of successive reduced values from:
--
-- > scan f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
scan :: (a -> b -> a) -> a -> Stream b -> Stream a
scan f z (Cons x xs) =  z <:> scan f (f z x) xs

-- | @scan'@ is a strict scan.
scan' :: (a -> b -> a) -> a -> Stream b -> Stream a
scan' f z xs =  z <:> (scan' f $! (f z (head xs))) (tail xs)

-- | 'scan1' is a variant of 'scan' that has no starting value argument:
--
-- > scan1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scan1 :: (a -> a -> a) -> Stream a -> Stream a
scan1 f (Cons x xs) = scan f x xs

-- | @scan1'@ is a strict scan that has no starting value.
scan1' :: (a -> a -> a) -> Stream a -> Stream a
scan1' f (Cons x xs) = scan' f x xs

-- | 'transpose' computes the transposition of a stream of streams.
transpose :: Stream (Stream a) -> Stream (Stream a)
transpose (Cons (Cons x xs) yss) =
    (x <:> map head yss) <:> transpose (xs <:> map tail yss)

-- | 'iterate' @f@ @x@ function produces the infinite sequence
-- of repeated applications of @f@ to @x@.
--
-- > iterate f x = [x, f x, f (f x), ..]
iterate :: (a -> a) -> a -> Stream a
iterate f x = Cons x (iterate f (f x))

-- | 'repeat' @x@ returns a constant stream, where all elements are
-- equal to @x@.
repeat :: a -> Stream a
repeat x = Cons x (repeat x)

-- | 'cycle' @xs@ returns the infinite repetition of @xs@:
--
-- > cycle [1,2,3] = Cons 1 (Cons 2 (Cons 3 (Cons 1 (Cons 2 ...
cycle :: [a] -> Stream a
cycle xs = foldr Cons (cycle xs) xs

-- | The unfold function is similar to the unfold for lists. Note
-- there is no base case: all streams must be infinite.
unfold :: (c -> (a,c)) -> c -> Stream a
unfold f c =
  let (x,d) = f c
  in Cons x (unfold f d)

-- | 'take' @n@ @xs@ returns the first @n@ elements of @xs@.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
take :: Int -> Stream a  -> [a]
take n (Cons x xs)
  | n == 0    = []
  | n > 0     =  x : (take (n - 1) xs)
  | otherwise = error "Stream.take: negative argument."

-- | 'drop' @n@ @xs@ drops the first @n@ elements off the front of
-- the sequence @xs@.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
drop :: Int -> Stream a -> Stream a
drop n xs
  | n == 0    = xs
  | n > 0     = drop (n - 1) (tail xs)
  | otherwise = error "Stream.drop: negative argument."

-- | The 'splitAt' function takes an integer @n@ and a stream @xs@
-- and returns a pair consisting of the prefix of @xs@ of length
-- @n@ and the remaining stream immediately following this prefix.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
splitAt :: Int -> Stream a -> ([a], Stream a)
splitAt n xs
  | n == 0    = ([],xs)
  | n > 0     = let (prefix,rest) = splitAt (n-1) (tail xs)
                in (head xs : prefix, rest)
  | otherwise = error "Stream.splitAt negative argument."

-- | 'takeWhile' @p@ @xs@ returns the longest prefix of the stream
-- @xs@ for which the predicate @p@ holds.
takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p (Cons x xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

-- | 'dropWhile' @p@ @xs@ returns the suffix remaining after
-- 'takeWhile' @p@ @xs@.
--
-- /Beware/: this function may diverge if every element of @xs@
-- satisfies @p@, e.g.  @dropWhile even (repeat 0)@ will loop.
dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile p (Cons x xs)
  | p x       = dropWhile p xs
  | otherwise = Cons x xs

-- | 'span' @p@ @xs@ returns the longest prefix of @xs@ that satisfies
-- @p@, together with the remainder of the stream.
span :: (a -> Bool) -> Stream a -> ([a], Stream a)
span p (Cons x xs)
  | p x       = let (trues, falses) = span p xs
                in (x : trues, falses)
  | otherwise = ([], Cons x xs)

-- | The 'break' @p@ function is equivalent to 'span' @not . p@.
break :: (a -> Bool) -> Stream a -> ([a], Stream a)
break p = span (not . p)

-- | 'filter' @p@ @xs@, removes any elements from @xs@ that do not satisfy @p@.
--
-- /Beware/: this function may diverge if there is no element of
-- @xs@ that satisfies @p@, e.g.  @filter odd (repeat 0)@ will loop.
filter :: (a -> Bool) -> Stream a -> Stream a
filter p (Cons x xs)
  | p x       = Cons x (filter p xs)
  | otherwise = filter p xs

-- | The 'partition' function takes a predicate @p@ and a stream
-- @xs@, and returns a pair of streams. The first stream corresponds
-- to the elements of @xs@ for which @p@ holds; the second stream
-- corresponds to the elements of @xs@ for which @p@ does not hold.
--
-- /Beware/: One of the elements of the tuple may be undefined. For
-- example, @fst (partition even (repeat 0)) == repeat 0@; on the
-- other hand @snd (partition even (repeat 0))@ is undefined.
partition :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
partition p (Cons x xs) =
  let (trues,falses) = partition p xs
  in if p x then (Cons x trues, falses)
            else (trues, Cons x falses)

-- | The 'group' function takes a stream and returns a stream of
-- lists such that flattening the resulting stream is equal to the
-- argument.  Moreover, each sublist in the resulting stream
-- contains only equal elements.  For example,
--
-- > group $ cycle "Mississippi" = "M" ::: "i" ::: "ss" ::: "i" ::: "ss" ::: "i" ::: "pp" ::: "i" ::: "M" ::: "i" ::: ...
group :: Eq a => Stream a -> Stream [a]
group (Cons x ys) = let (xs, zs) = span (\y -> x == y) ys
                    in (x : xs) <:> group zs

-- | The 'isPrefix' function returns @True@ if the first argument is
-- a prefix of the second.
isPrefixOf :: Eq a => [a] -> Stream a -> Bool
isPrefixOf [] _ = True
isPrefixOf (y:ys) (Cons x xs)
  | y == x    = isPrefixOf ys xs
  | otherwise = False

-- | @xs !! n@ returns the element of the stream @xs@ at index
-- @n@. Note that the head of the stream has index 0.
--
-- /Beware/: passing a negative integer as the first argument will cause
-- an error.
(!!) :: Stream a -> Int -> a
(!!) (Cons x xs) n
  | n == 0    = x
  | n > 0     = xs !! (n - 1)
  | otherwise = error "Stream.!! negative argument"

-- | The 'elemIndex' function returns the index of the first element
-- in the given stream which is equal (by '==') to the query element,
--
-- /Beware/: 'elemIndex' @x@ @xs@ will diverge if none of the elements
-- of @xs@ equal @x@.
elemIndex :: Eq a => a -> Stream a -> Int
elemIndex x = findIndex (\y -> x == y)

-- | The 'elemIndices' function extends 'elemIndex', by returning the
-- indices of all elements equal to the query element, in ascending order.
--
-- /Beware/: 'elemIndices' @x@ @xs@ will diverge if any suffix of
-- @xs@ does not contain @x@.
elemIndices :: Eq a => a -> Stream a -> Stream Int
elemIndices x = findIndices (x==)


-- | The 'findIndex' function takes a predicate and a stream and returns
-- the index of the first element in the stream that satisfies the predicate,
--
-- /Beware/: 'findIndex' @p@ @xs@ will diverge if none of the elements of
-- @xs@ satisfy @p@.
findIndex :: (a -> Bool) -> Stream a -> Int
findIndex p = indexFrom 0
    where
    indexFrom ix (Cons x xs) 
      | p x       = ix
      | otherwise = (indexFrom $! (ix + 1)) xs

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending
-- order.
--
-- /Beware/: 'findIndices' @p@ @xs@ will diverge if all the elements
-- of any suffix of @xs@ fails to satisfy @p@.
findIndices :: (a -> Bool) -> Stream a -> Stream Int
findIndices p = indicesFrom 0
    where
    indicesFrom ix (Cons x xs) = 
      let ixs = (indicesFrom $! (ix+1)) xs
      in if p x then Cons ix ixs else ixs

-- | The 'zip' function takes two streams and returns a list of
-- corresponding pairs.
zip :: Stream a -> Stream b -> Stream (a,b)
zip (Cons x xs) (Cons y ys) = Cons (x,y) (zip xs ys)

-- | The 'zipWith' function generalizes 'zip'. Rather than tupling
-- the functions, the elements are combined using the function
-- passed as the first argument to 'zipWith'.
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

-- | The 'unzip' function is the inverse of the 'zip' function.
unzip :: Stream (a,b) -> (Stream a, Stream b)
unzip (Cons (x,y) xys) = (Cons x (fst (unzip xys)), Cons y (snd (unzip xys)))

-- | The 'words' function breaks a stream of characters into a
-- stream of words, which were delimited by white space.
--
-- /Beware/: if the stream of characters @xs@ does not contain white
-- space, accessing the tail of @words xs@ will loop.
words :: Stream Char -> Stream String
words xs = let (w, ys) = break isSpace xs
                 in Cons w (words ys)

-- | The 'unwords' function is an inverse operation to 'words'. It
-- joins words with separating spaces.
unwords :: Stream String -> Stream Char
unwords (Cons x xs) = foldr Cons (Cons ' ' (unwords xs)) x

-- | The 'lines' function breaks a stream of characters into a list
-- of strings at newline characters. The resulting strings do not
-- contain newlines.
--
-- /Beware/: if the stream of characters @xs@ does not contain
-- newline characters, accessing the tail of @lines xs@ will loop.
lines :: Stream Char -> Stream String
lines xs = let (l, ys) = break (== '\n') xs
                 in Cons l (lines (tail ys))

-- | The 'unlines' function is an inverse operation to 'lines'. It
-- joins lines, after appending a terminating newline to each.
unlines :: Stream String -> Stream Char
unlines (Cons x xs) = foldr Cons (Cons '\n' (unlines xs)) x

-- | The 'toList' converts a stream into an infinite list.
toList :: Stream a -> [a]
toList (Cons x xs) = x : toList xs

-- | The 'fromList' converts an infinite list to a
-- stream.
--
-- /Beware/: Passing a finite list, will cause an error.
fromList :: [a] -> Stream a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = error "Stream.listToStream applied to finite list"
