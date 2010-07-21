{-# LANGUAGE ScopedTypeVariables #-}
module Ros.Util.BytesToVector (bytesToVector, bytesToVectorL, 
                               vectorToBytes) where
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.ForeignPtr (newForeignPtr_, withForeignPtr)
import Foreign.Storable (sizeOf)
import System.IO.Unsafe (unsafePerformIO)

-- |Construct a Storable Vector with the specified number of items
-- from a ByteString without copying.
bytesToVector :: V.Storable a => Int -> BS.ByteString -> V.Vector a
bytesToVector n bs = unsafePerformIO $
                     BS.useAsCStringLen bs $
                     (\(p,len) -> do fp <- newForeignPtr_ (castPtr p)
                                     let v = VM.unsafeFromForeignPtr fp 0 n
                                     G.unsafeFreeze v)

bytesToVectorL :: V.Storable a => Int -> BL.ByteString -> V.Vector a
bytesToVectorL n = bytesToVector n . BS.concat . BL.toChunks

-- |Construct a ByteString from a Storable Vector. This does involve
-- copying the underlying bytes.
vectorToBytes :: forall a. V.Storable a => V.Vector a-> BS.ByteString
vectorToBytes v = unsafePerformIO $
                  withForeignPtr fp (\p -> let p' = castPtr $ plusPtr p offset
                                           in BS.packCStringLen (p', len*sz))
    where (fp,offset,len) = V.unsafeToForeignPtr v
          sz = sizeOf (undefined::a)


