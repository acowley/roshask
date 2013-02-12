{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
-- |Binary serialization/deserialization utilities for types used in
-- ROS messages. This module is used by generated code for .msg types.
-- NOTE: The native byte ordering of the host is used to support the
-- common scenario of same-machine transport.
module Ros.Internal.RosBinary where
import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import qualified Data.Vector.Storable as V
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Foreign.Storable (sizeOf, Storable)

import Ros.Internal.RosTypes
import Ros.Internal.Util.BytesToVector

-- |A type class for binary serialization of ROS messages. Very like
-- the standard Data.Binary type class, but with different, more
-- compact, instances for base types and an extra class method for
-- dealing with message headers.
class RosBinary a where
    -- |Serialize a value to a ByteString.
    put :: a -> Put
    -- |Deserialize a value from a ByteString.
    get :: Get a
    -- |Serialize a ROS message given a sequence number. This number
    -- may be used by message types with headers. The default
    -- implementation ignores the sequence number.
    putMsg :: Word32 -> a -> Put
    putMsg _ = put

instance RosBinary Bool where
    put True = putWord8 1
    put False = putWord8 0
    get = (> 0) <$> getWord8

instance RosBinary Int8 where
    put = putWord8 . fromIntegral
    get = fromIntegral <$> getWord8

instance RosBinary Word8 where
    put = putWord8
    get = getWord8

instance RosBinary Int16 where
    put = putWord16host . fromIntegral
    get = fromIntegral <$> getWord16host

instance RosBinary Word16 where
    put = putWord16host
    get = getWord16host

instance RosBinary Int where
    put = putWord32host . fromIntegral
    get = fromIntegral <$> getWord32host

instance RosBinary Word32 where
    put = putWord32host
    get = getWord32host

instance RosBinary Int64 where
    put = putWord64host . fromIntegral
    get = fromIntegral <$> getWord64host

instance RosBinary Word64 where
    put = putWord64host
    get = getWord64host

instance RosBinary Float where
    put = putWord32le . unsafeCoerce
    get = unsafeCoerce <$> getWord32le

instance RosBinary Double where
    put = putWord64le . unsafeCoerce
    get = unsafeCoerce <$> getWord64le

getAscii :: Get Char
getAscii = toEnum . fromEnum <$> getWord8

putAscii :: Char -> Put
putAscii = putWord8 . toEnum . fromEnum

putUnit :: Put
putUnit = putWord8 0

getUnit :: Get ()
getUnit = getWord8 >> return ()

instance RosBinary String where
    put s = let s' = BC8.pack s
            in putInt32 (BC8.length s') >> putByteString s'
    get = getInt32 >>= (BC8.unpack <$>) . getByteString

instance RosBinary B.ByteString where
    put b = putInt32 (B.length b) >> putByteString b
    get = getInt32 >>= getByteString

instance RosBinary ROSTime where
    put (s,n) = putWord32host s >> putWord32host n
    get = (,) <$> getWord32host <*> getWord32host

putList :: RosBinary a => [a] -> Put
putList xs = putInt32 (length xs) >> mapM_ put xs

getList :: RosBinary a => Get [a]
getList = getInt32 >>= flip replicateM get

putFixedList :: RosBinary a => [a] -> Put
putFixedList = mapM_ put

getFixedList :: RosBinary a => Int -> Get [a]
getFixedList = flip replicateM get

{-
instance RosBinary ROSDuration where
    put (s,n) = putWord32host s >> putWord32host n
    get =  (,) <$> getWord32host <*> getWord32host
-}

getInt32 :: Get Int
getInt32 = fromIntegral <$> getWord32le

putInt32 :: Int -> Put 
putInt32 = putWord32le . fromIntegral

instance (RosBinary a, Storable a) => RosBinary (V.Vector a) where
    put v = putInt32 (V.length v) >> putByteString (vectorToBytes v)
    get = getInt32 >>= getFixed

getFixed :: forall a. Storable a => Int -> Get (V.Vector a)
getFixed n = bytesToVector n <$> getByteString (n*(sizeOf (undefined::a)))

putFixed :: (Storable a, RosBinary a) => V.Vector a -> Put
putFixed = putByteString . vectorToBytes
