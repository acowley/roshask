-- Note that tne endianess of message serialization is unclear. I am
-- using the native byte ordering of the host to support the common
-- scenario of same-machine transport.
module RosBinary where
import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>))
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Word
import Unsafe.Coerce (unsafeCoerce)
import ROSTypes

getBool :: Get Bool
getBool = (> 0) <$> getWord8

putBool :: Bool -> Put 
putBool True = putWord8 1
putBool False = putWord8 0

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

putInt8 :: Int8 -> Put
putInt8 = putWord8 . fromIntegral

getUInt8 :: Get Word8
getUInt8 = getWord8
putUInt8 = putWord8

getInt16 :: Get Int16
getInt16 = fromIntegral <$> getWord16host

putInt16 :: Int16 -> Put
putInt16 = putWord16host . fromIntegral

getUInt16 :: Get Word16
getUInt16 = getWord16host

putUInt16 :: Word16 -> Put
putUInt16 = putWord16host

getInt32 :: Get Int
getInt32 = fromIntegral <$> getWord32host

putInt32 :: Int -> Put
putInt32 = putWord32host . fromIntegral

getUInt32 :: Get Word32
getUInt32 = getWord32host

putUInt32 :: Word32 -> Put
putUInt32 = putWord32host

getInt64 :: Get Int64
getInt64 = fromIntegral <$> getWord64host

putInt64 :: Int64 -> Put
putInt64 = putWord64host . fromIntegral

getUInt64 :: Get Word64
getUInt64 = getWord64host

putUInt64 :: Word64 -> Put
putUInt64 = putWord64host

getFloat32 :: Get Float
getFloat32 = unsafeCoerce <$> getWord32le

putFloat32 :: Float -> Put
putFloat32 = putWord32le . unsafeCoerce

getFloat64 :: Get Double
getFloat64 = unsafeCoerce <$> getWord64le

putFloat64 :: Double -> Put
putFloat64 = putWord64le . unsafeCoerce

getAscii :: Get Char
getAscii = toEnum . fromEnum <$> getWord8

putAscii :: Char -> Put
putAscii = putWord8 . toEnum . fromEnum

getString :: Get String
getString = go "" 
    where go s = do c <- getAscii
                    if c == '\NUL' then return (reverse s) else go (c:s)

putString :: String -> Put
putString = mapM_ putAscii >=> const (putAscii '\NUL')

getTime :: Get ROSTime
getTime = (,) <$> getWord32host <*> getWord32host

putTime :: ROSTime -> Put
putTime (s,n) = putWord32host s >> putWord32host n

getDuration :: Get ROSDuration
getDuration = (,) <$> getWord32host <*> getWord32host

putDuration :: ROSDuration -> Put
putDuration (s,n) = putWord32host s >> putWord32host n

