{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
-- |Binary serialization deserialization utilities for types used in
-- ROS messages. This module is used by generated code for .msg
-- types.

-- Note that the endianess of message serialization is unclear. I am
-- using the native byte ordering of the host to support the common
-- scenario of same-machine transport.
module Ros.RosBinary where
import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>))
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import qualified Data.Vector.Unboxed as V
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Char8 as BC8
import Foreign.C.String (CStringLen)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (sizeOf, Storable, peekElemOff)
import System.IO.Unsafe

import Ros.RosTypes

class BinaryCompact a where
    put :: a -> Put
    get :: Get a

instance BinaryCompact Bool where
    put True = putWord8 1
    put False = putWord8 0
    get = (> 0) <$> getWord8

instance BinaryCompact Int8 where
    put = putWord8 . fromIntegral
    get = fromIntegral <$> getWord8

instance BinaryCompact Word8 where
    put = putWord8
    get = getWord8

instance BinaryCompact Int16 where
    put = putWord16host . fromIntegral
    get = fromIntegral <$> getWord16host

instance BinaryCompact Word16 where
    put = putWord16host
    get = getWord16host

instance BinaryCompact Int where
    put = putWord32host . fromIntegral
    get = fromIntegral <$> getWord32host

instance BinaryCompact Word32 where
    put = putWord32host
    get = getWord32host

instance BinaryCompact Int64 where
    put = putWord64host . fromIntegral
    get = fromIntegral <$> getWord64host

instance BinaryCompact Word64 where
    put = putWord64host
    get = getWord64host

instance BinaryCompact Float where
    put = putWord32le . unsafeCoerce
    get = unsafeCoerce <$> getWord32le

instance BinaryCompact Double where
    put = putWord64le . unsafeCoerce
    get = unsafeCoerce <$> getWord64le

getAscii :: Get Char
getAscii = toEnum . fromEnum <$> getWord8

putAscii :: Char -> Put
putAscii = putWord8 . toEnum . fromEnum

instance BinaryCompact String where
    put s = let s' = BC8.pack s
            in putInt32 (BC8.length s') >> putByteString s'
    get = getInt32 >>= (BC8.unpack <$>) . getByteString
{-
    put = mapM_ putAscii >=> const (putAscii '\NUL')
    get = go ""
        where go s = do c <- getAscii
                        if c == '\NUL' then return (reverse s) else go (c:s)
-}

instance BinaryCompact B.ByteString where
    put b = putInt32 (B.length b) >> putByteString b
    get = getInt32 >>= getByteString

instance BinaryCompact ROSTime where
    put (s,n) = putWord32host s >> putWord32host n
    get = (,) <$> getWord32host <*> getWord32host

{-
instance BinaryCompact ROSDuration where
    put (s,n) = putWord32host s >> putWord32host n
    get =  (,) <$> getWord32host <*> getWord32host
-}

bytesToVec :: (Storable a, V.Unbox a) => a -> B.ByteString -> V.Vector a
bytesToVec x bs = unsafePerformIO $ BU.unsafeUseAsCStringLen bs go
    where go (ptr,len) = let ptr' = castPtr ptr
                             num = len `div` (sizeOf x)
                         in return $ 
                            V.generate num (unsafePerformIO . peekElemOff ptr')

getInt32 = fromIntegral <$> getWord32le
putInt32 = putWord32le . fromIntegral

instance (BinaryCompact a, Storable a, V.Unbox a) => 
         BinaryCompact (V.Vector a) where
    put v = putInt32 (V.length v) >> V.mapM_ put v
    get = getInt32 >>= getFixed

getFixed :: forall a. (Storable a, V.Unbox a) => Int -> Get (V.Vector a)
getFixed n = bytesToVec undefined <$> getBytes (n*(sizeOf (undefined::a)))

putFixed :: (BinaryCompact a, V.Unbox a) => V.Vector a -> Put
putFixed = V.mapM_ put
