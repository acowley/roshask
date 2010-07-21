{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, BangPatterns, 
             TypeSynonymInstances #-}
-- |Binary iteratee-style serialization helpers for working with ROS
-- message types. This module is used by the automatically-generated
-- code for ROS .msg types.

module Ros.BinaryIter (Iter(..), BinaryIter(..), streamIn, consume') where
import Control.Applicative
import Control.Monad.ST (runST)
import Data.Binary (Binary)
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import Data.Int
import Data.Monoid
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VM
import Data.Word
import Foreign.Storable
import GHC.Int (Int64)
import System.IO (Handle)
import System.IO.Unsafe (unsafeInterleaveIO)
import Unsafe.Coerce (unsafeCoerce)
import Ros.RosTypes

-- |An Iter provides either a continuation asking for more data or a
-- produced value along with another Iter of the same type.
data Iter a b = More (a -> Iter a b) | Emit b a --(Iter a b)

-- Note: This Functor instance is rather funny in that the function is
-- lost in the continuation of the Emit variant.
instance Functor (Iter a) where
    fmap f (More k) = More (fmap f . k)
    fmap f (Emit v b) = Emit (f v) b

instance Monoid a => Applicative (Iter a) where
    pure f = Emit f mempty
    More k <*> b2 = More (\bs -> k bs <*> b2)
    Emit f r <*> More k = More (\bs -> Emit f mempty <*> k (r `mappend` bs))
    Emit f r1 <*> Emit v r2 = Emit (f v) (r1 `mappend` r2)

-- |This is like Get, but augmented to support partial reads via the
-- Iter datatype.
class BinaryIter a where
    consume :: ByteString -> Iter ByteString a

-- |Use a type's BinaryIter instance to construct an Iter value.
consume' :: BinaryIter a => Iter ByteString a
consume' = More consume

-- The maximum number of bytes read from the Handle at a time.
cHUNK_SIZE :: Int
cHUNK_SIZE = 16 * 1024

-- |The function that does the work of streaming members of the
-- BinaryIter class in from a Handle.
streamIn :: BinaryIter a => Handle -> IO (Stream a)
streamIn h = go consume
    where -- go k = do bs <- B.hGetNonBlocking h cHUNK_SIZE 
          --           if B.null bs
          --             then B.hGet h 4 >>= emit . k
          --             else emit (k bs)
          go k = do len <- B.hGet h 4
                    B.hGet h (runGet getInt len) >>= emit . k
          emit (More k')   = go k'
          emit (Emit x k') = Stream x <$> unsafeInterleaveIO (emit (consume k'))

-- Unsafe getters for the most common value sizes.
unsafeGet :: Int -> Get a
unsafeGet 1 = unsafeCoerce <$> getWord8
unsafeGet 2 = unsafeCoerce <$> getWord16host
unsafeGet 4 = unsafeCoerce <$> getWord32host
unsafeGet 8 = unsafeCoerce <$> getWord64host

{-# SPECIALIZE getStorable :: ByteString -> Iter ByteString Word8  #-}
{-# SPECIALIZE getStorable :: ByteString -> Iter ByteString Word16 #-}
{-# SPECIALIZE getStorable :: ByteString -> Iter ByteString Int    #-}
{-# SPECIALIZE getStorable :: ByteString -> Iter ByteString Float  #-}
{-# SPECIALIZE getStorable :: ByteString -> Iter ByteString Double #-}

-- Storables may all be made instances of the BinaryIter class in the
-- same way.
getStorable :: forall a. Storable a => ByteString -> Iter ByteString a
getStorable bs = let sz = fromIntegral $ sizeOf (undefined::a)
                     get' = unsafeGet (fromIntegral sz)
                 in if B.length bs < sz
                    then More (getStorable . B.append bs)
                    else let (h,t) = B.splitAt sz bs
                         in Emit (runGet get' h) t --(getStorable t)

instance BinaryIter Bool   where consume = getStorable
instance BinaryIter Int8   where consume = getStorable
instance BinaryIter Int16  where consume = getStorable
instance BinaryIter Word8  where consume = getStorable
instance BinaryIter Word16 where consume = getStorable
instance BinaryIter Int    where consume = getStorable
instance BinaryIter Word32 where consume = getStorable
instance BinaryIter Int64  where consume = getStorable
instance BinaryIter Word64 where consume = getStorable
instance BinaryIter Float  where consume = getStorable
instance BinaryIter Double where consume = getStorable

-- Build a vector of n elements from the given bytestring.
buildVector :: V.Unbox a => Int -> ByteString -> Get a -> V.Vector a
buildVector n bs g = 
    runST $ 
    do m <- VM.new n
       let go !i !bs | i == n    = VG.unsafeFreeze m
                     | otherwise = let (x,t,_) = runGetState g bs 0
                                   in VM.unsafeWrite m i x >> go (i+1) t
       go 0 bs

getInt = unsafeCoerce <$> getWord32host

-- Build Vectors by reading in the number of elements and then the
-- elements themselves. The vector is only returned when all data is
-- available.
instance forall a. (V.Unbox a, Storable a) => BinaryIter (V.Vector a) where
    consume bs = if B.length bs < 4
                 then More (consume . B.append bs)
                 else let (h,t) = B.splitAt 4 bs
                      in getCount (runGet getInt h) t
        where sz = sizeOf (undefined::a)
              get' = unsafeGet sz
              getCount n bs = 
                  let len = fromIntegral $ B.length bs
                  in if len < n * sz
                     then More (getCount n . B.append bs)
                     else let (h,t) = B.splitAt (fromIntegral (n*sz)) bs
                              v = buildVector n h get'
                          in Emit v t --(consume t)

instance BinaryIter BC8.ByteString where
    consume bs = if B.length bs < 4
                 then More (consume . B.append bs)
                 else let (h,t) = B.splitAt 4 bs
                      in getCount (fromIntegral (runGet getInt h)) t
        where getCount n bs = 
                  if B.length bs < n then More (getCount n . B.append bs)
                  else let (h,t) = B.splitAt n bs
                       in Emit h t --(consume t)

instance (BinaryIter a, BinaryIter b) => BinaryIter (a,b) where
    consume = case (,) <$> consume' <*> consume' of
                Emit v r1 -> \r2 -> Emit v (r1 `mappend` r2)
                More k -> k

instance BinaryIter String where
    consume = iterUnpack . consume
        where iterUnpack (More k) = More (iterUnpack . k)
              iterUnpack (Emit x t) = Emit (BC8.unpack x) t
                         